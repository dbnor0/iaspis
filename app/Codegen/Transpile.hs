{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Transpile where

import Iaspis.Grammar as I hiding (proxyDecls, facetDecls)
import Codegen.Types as S
import Solidity.Grammar as S
import Yul.Grammar as Y
import Iaspis.DeclUtils
import Data.Maybe
import Data.List.Extra
import Codegen.Utils
import Lens.Micro.Platform (_1, _2, (^.))

transpile :: [I.Module] -> [S.Module]
transpile ms =
  let cs = contractDecls =<< ms
      fs = facetDecls =<< ms
      ps = proxyDecls (snd <$> fs) =<< ms
      storageM = Just $ storageModule ps
  in catMaybes $ (transpileContract <$> cs) <> (transpileProxy <$> ps) <> (transpileFacet <$> fs) <> [storageM]

storageModule :: [([I.Import], I.Contract, [Facet])] -> S.Module
storageModule ps =
  S.Module
  { S.imports = importIds =<< ((^. _1) =<< ps)
  , moduleId = "StorageStructs"
  , decls = [S.LibraryDef libraryDef]
  }
  where libraryDef = LibraryDefinition "StorageStructs" (storageModuleElems . (^. _2) =<< ps)

storageModuleElems :: Contract -> [S.ContractBodyElem]
storageModuleElems (ProxyContract _ _ _ fs) = toElems . storageTuple =<< fieldMappings
  where toElems = \(v, s, f) -> [StateVarDecl v, StructDef s, FunctionDef f]
        fieldMappings = groupSort . catMaybes $ kindToFacetId <$> fs
        kindToFacetId f@Field{ fieldProxyKind } =
          case fieldProxyKind of
            Nothing -> Nothing
            (Just SharedProxyMember) -> Just ("global", f)
            (Just (UniqueProxyMember id)) -> Just (id, f)
storageModuleElems _ = []

storageTuple :: (S.Identifier, [Field]) -> (StateVarDeclaration, StructDefinition, FunctionDefinition)
storageTuple (sName, fs) = (ptrDecl, structDef, fnDef)
  where ptrDecl = StateVarDeclaration
                  { stateVarType = bytes 32
                  , stateVarVisibility = S.Public
                  , stateVarModifier = Just Constant
                  , stateVarId = ptrName
                  , stateVarInitializer = Just ptrExpr
                  }
        ptrName = sName <> "_storage_position"
        ptrExpr = S.FunctionCallE (S.IdentifierE "keccak256") [S.LiteralE (S.StringLit (sName <> "_storage_struct"))]
        structDef = S.StructDefinition (sName <> "_storage_struct") (structMember <$> fs)
        structMember Field{fieldName, fieldType} = StructMember (transpileType fieldType) fieldName
        fnDef = S.FunctionDefinition
                { functionId = sName <> "Storage"
                , S.functionVisibility = S.Internal
                , S.functionMutability = S.Pure
                , functionPayablity = False
                , functionVirtualSpec = False
                , functionOverrideSpec = False
                , S.functionArgs = []
                , S.functionReturnType = [S.FunctionArg (struct (sName <> "_storage_struct")) S.Storage "ds"]
                , S.functionBody = fnBody
                }
        fnBody = [ S.VarDeclStmt (S.FunctionArg (bytes 32) S.Memory "position") (Just (S.IdentifierE ptrName))
                 , S.AssemblyStmt (Y.AssignmentStmt (Y.PathE (Y.IdentifierE "ds") (Y.IdentifierE "slot")) (Y.IdentifierE "position"))
                 ]

transpileContract :: ([I.Import], I.Contract) -> Maybe S.Module
transpileContract (is, I.ImmutableContract { contractName, contractFields, contractFns }) =
  Just $ S.Module { moduleId=contractName, S.imports=importIds =<< is, decls=[ContractDef contractDef] }
  where contractDef =
          ContractDefinition
          { contractAbstractSpec = False
          , contractId = contractName
          , contractInheritanceList = []
          , contractBody = (StateVarDecl . transpileContractField <$> contractFields) <> (fns <$> contractFns)
          }
        fns f@(Function (FunctionHeader _ _ _ "constructor" _ _ _) _) = (ConstructorDef . transpileFunctionDef) f
        fns f@(Function (FunctionHeader _ _ _ "fallback" _ _ _) _) = (FallbackDef . transpileFunctionDef) f
        fns f@(Function (FunctionHeader _ _ _ "receive" _ _ _) _) = (ReceiveDef . transpileFunctionDef) f
        fns f = (FunctionDef . transpileFunctionDef) f
transpileContract _ = Nothing

transpileContractField :: I.Field -> S.StateVarDeclaration
transpileContractField Field{ fieldType, fieldVisibility, fieldName, fieldInitializer } =
  StateVarDeclaration
    { stateVarType = transpileType fieldType
    , stateVarVisibility = transpileVisibility fieldVisibility
    , stateVarModifier = Nothing
    , stateVarId = fieldName
    , stateVarInitializer = transpileExpr <$> fieldInitializer
    }

transpileFunctionDef :: I.Function -> S.FunctionDefinition
transpileFunctionDef Function{ functionHeader, I.functionBody } =
  FunctionDefinition
    { S.functionId = functionName functionHeader
    , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility functionHeader
    , S.functionMutability = transpileMutability $ I.functionMutability functionHeader
    , S.functionPayablity = transpilePayability $ I.functionPayability functionHeader
    , S.functionVirtualSpec = False
    , S.functionOverrideSpec = False
    , S.functionArgs = transpileFunctionArg <$> I.functionArgs functionHeader
    , S.functionReturnType = [transpileReturnType $ I.functionReturnType functionHeader]
    , S.functionBody = transpileStmt <$> functionBody
    }

transpileFunctionArg :: I.Field -> S.FunctionArg
transpileFunctionArg Field{fieldType, fieldLocation, fieldName} =
  FunctionArg
    { functionArgType = transpileType fieldType
    , functionArgLocation = transpileLocation fieldLocation
    , functionArgId = fieldName
    }

transpileReturnType :: I.Type -> S.FunctionArg
transpileReturnType t =
  FunctionArg
    { functionArgType = transpileType t
    , functionArgLocation = S.Memory
    , functionArgId = ""
    }

transpileStmt :: I.Statement -> S.Statement
transpileStmt = \case
  I.VarDeclStmt f _ e -> S.VarDeclStmt (transpileFunctionArg f) (Just $ transpileExpr e)
  I.AssignmentStmt id _ e -> S.AssignmentStmt (transpileExpr id) (transpileExpr e)
  I.ReturnStmt e -> S.ReturnStmt $ transpileExpr <$> e
  I.IfStmt cond b1 b2 -> S.IfStmt (transpileExpr cond) (transpileStmt b1) (transpileStmt <$> b2)
  I.BlockStmt stmts -> S.BlockStmt $ transpileStmt <$> stmts
  I.BreakStmt -> S.BreakStmt
  I.ContinueStmt -> S.ContinueStmt
  I.ExpressionStmt e -> S.ExpressionStmt (transpileExpr e)

transpileExpr :: I.Expression -> S.Expression
transpileExpr = \case
  I.LiteralE v -> S.LiteralE $ transpileValue v
  I.IdentifierE id -> S.IdentifierE id
  I.FunctionCallE id es -> S.FunctionCallE (S.IdentifierE id) (transpileExpr <$> es)
  I.InstantiationE id es -> S.InstantiationE (S.IdentifierE id) (transpileExpr <$> es)
  I.UnaryE op e -> S.UnaryE (transpileUnaryOp op) (transpileExpr e)
  I.BinaryE op e1 e2 -> S.BinaryE (transpileBinaryOp op) (transpileExpr e1) (transpileExpr e2)

transpileUnaryOp :: I.UnaryOp -> S.UnaryOp
transpileUnaryOp = \case
  I.ArithmeticNegationOp -> S.ArithmeticNegationOp
  I.LogicalNegationOp -> S.LogicalNegationOp
  I.BitwiseNegationOp -> S.BitwiseNegationOp
  I.IncrementOp -> S.IncrementOp
  I.DecrementOp -> S.DecrementOp

transpileBinaryOp :: I.BinaryOp -> S.BinaryOp
transpileBinaryOp = \case
  I.AdditionOp -> S.AdditionOp
  I.SubtractionOp -> S.SubtractionOp
  I.MultiplicationOp -> S.MultiplicationOp
  I.DivisionOp -> S.DivisionOp
  I.ModuloOp -> S.ModuloOp
  I.ConjunctionOp -> S.ConjunctionOp
  I.DisjunctionOp -> S.DisjunctionOp
  I.EqualityOp -> S.EqualityOp
  I.InequalityOp -> S.InequalityOp
  I.LessThanOp -> S.LessThanOp
  I.GreaterThanOp -> S.GreaterThanOp
  I.LessThanEqualOp -> S.LessThanEqualOp
  I.GreaterThanEqualOp -> S.GreaterThanEqualOp
  I.LeftShiftOp -> S.LeftShiftOp
  I.RightShiftOp -> S.RightShiftOp
  I.BitwiseConjunctionOp -> S.BitwiseConjunctionOp
  I.BitwiseDisjunctionOp -> S.BitwiseDisjunctionOp
  I.BitwiseExclDisjunctionOp -> S.BitwiseExclDisjunctionOp

transpileProxy :: ([I.Import], I.Contract, [Facet]) -> Maybe S.Module
transpileProxy (is, p@I.ProxyContract { proxyName, facetList }, facets) =
  Just $ S.Module { moduleId=proxyName, S.imports=is', decls=proxyDecl }
  where proxyDecl = transpileProxyContract p facets
        is' = defaultProxyImports <> (importIds =<< is) <> facetList
transpileProxy _ = Nothing

transpileProxyContract :: I.Contract -> [Facet] -> [S.Declaration]
transpileProxyContract (I.ProxyContract _ pId _ _) facets = [ContractDef contractDef]
  where contractDef = ContractDefinition False pId [] contractBody
        contractBody =
          [ transpileProxyConstructor facets
          , transpileProxyFallback
          , transpileProxyReceive
          ]
transpileProxyContract _ _ = []

transpileProxyConstructor :: [Facet] -> ContractBodyElem
transpileProxyConstructor facets = ConstructorDef $ FunctionDefinition
  { functionId = "constructor"
  , S.functionVisibility = S.Public
  , S.functionMutability = S.Mutable
  , functionPayablity = True
  , functionVirtualSpec = False
  , functionOverrideSpec = False
  , S.functionArgs = proxyConstructorArgs $ fst <$> facets
  , S.functionReturnType = [FunctionArg unit S.Memory ""]
  , S.functionBody = proxyConstructorBody facets
  }

proxyConstructorBody :: [Facet] -> [S.Statement]
proxyConstructorBody facets =
  [ S.ExpressionStmt $ S.FunctionCallE (S.IdentifierE "LibDiamond.setContractOwner") [S.IdentifierE "_contractOwner"]
  , S.VarDeclStmt
    (S.FunctionArg (array (struct "IDiamondCut.FacetCut") Nothing) S.Memory "cut")
    (Just $ S.ArrayInstantiationE "IDiamondCut.FacetCut" (S.LiteralE (S.NumberLit (length facets))))
  ]
  <>
  (proxyFacetCutInit =<< zip [0..n] facets)
  <>
  [ S.ExpressionStmt $ S.FunctionCallE (S.IdentifierE "LibDiamond.diamondCut")
    [ S.IdentifierE "cut"
    , S.FunctionCallE (S.IdentifierE "address") [S.LiteralE (S.NumberLit 0)]
    , S.LiteralE (S.StringLit "")
    ]
  ]
  where n = length facets

proxyFacetCutInit :: (Int, Facet) -> [S.Statement]
proxyFacetCutInit (idx, (fId, fFns)) =
  [ S.VarDeclStmt (S.FunctionArg (array (bytes 4) Nothing) S.Memory (fId <> "functionSelectors"))
    (Just $ S.ArrayInstantiationE "bytes4" (S.LiteralE (S.NumberLit 1)))
  ]
  <>
  (proxyFacetCutSelectors <$> zip3 [0..n] (replicate n fId) validFns)
  <>
  [S.AssignmentStmt (S.SubscriptE (S.IdentifierE "cut") (S.LiteralE (S.NumberLit idx)))
    (S.LiteralE (S.StructLit "IDiamondCut.FacetCut"
      [ ("facetAddress", S.IdentifierE $ "_" <> fId <> "Address")
      , ("action", S.IdentifierE "IDiamondCut.FacetCutAction.Add")
      , ("functionSelectors", S.IdentifierE $ fId <> "functionSelectors")
      ]
    ))
  ]
  where validFns = filter f fFns
        f = (||) <$> (== I.Public) . g <*> (== I.External) . g
        g = I.functionVisibility . I.functionHeader
        n = length validFns

proxyFacetCutSelectors :: (Int, S.Identifier, Function) -> S.Statement
proxyFacetCutSelectors (idx, fId, Function hd _) =
  S.AssignmentStmt
    (S.SubscriptE (S.IdentifierE (fId <> "functionSelectors")) (S.LiteralE (S.NumberLit idx)))
    (S.IdentifierE $ fId <> "." <> functionName hd <> ".selector")

proxyConstructorArgs :: [S.Identifier] -> [FunctionArg]
proxyConstructorArgs fIds = FunctionArg address S.Memory "_contractOwner" : (facetAddress <$> fIds)
  where facetAddress fId = FunctionArg address S.Memory ("_" <> fId <> "Address")

transpileProxyFallback :: ContractBodyElem
transpileProxyFallback = FallbackDef $ FunctionDefinition
  { functionId = "fallback"
  , S.functionVisibility = S.External
  , S.functionMutability = S.Mutable
  , functionPayablity = True
  , functionVirtualSpec = False
  , functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [FunctionArg unit S.Memory ""]
  , S.functionBody = proxyFallbackBody
  }

proxyFallbackBody :: [S.Statement]
proxyFallbackBody =
  [ S.VarDeclStmt (S.FunctionArg (struct "LibDiamond.DiamondStorage") S.Storage "ds") Nothing
  , S.VarDeclStmt (S.FunctionArg (bytes 32) S.Memory "position") (Just $ S.IdentifierE "LibDiamond.DIAMOND_STORAGE_POSITION")
  , S.AssemblyStmt (Y.AssignmentStmt (Y.PathE (Y.IdentifierE "ds") (Y.IdentifierE "slot")) (Y.IdentifierE "position"))
  , S.VarDeclStmt (S.FunctionArg address S.Memory "facet")
    (Just $ S.MemberAccessE (S.IdentifierE "ds")
      (S.MemberAccessE
        (S.SubscriptE
          (S.IdentifierE "selectorToFacetAndPosition") (S.MemberAccessE (S.IdentifierE "msg") (S.IdentifierE "sig"))
        )
        (S.IdentifierE "facetAddress")
      )
    )
  , S.ExpressionStmt $ S.FunctionCallE (S.IdentifierE "require")
    [ S.BinaryE S.InequalityOp (S.IdentifierE "facet") (S.FunctionCallE (S.IdentifierE "address") [S.LiteralE (S.NumberLit 0)])
    , S.LiteralE (S.StringLit "Diamond: Function does not exist")
    ]
  , S.AssemblyStmt $ Y.BlockStmt
    [ Y.ExpressionStmt $ Y.FunctionCallE (Y.IdentifierE "calldatacopy")
      [ Y.LiteralE (Y.NumberLit 0)
      , Y.LiteralE (Y.NumberLit 0)
      , Y.FunctionCallE (Y.IdentifierE "calldatasize") []
      ]
    , Y.VarDeclStmt "result" (Y.FunctionCallE (Y.IdentifierE "delegatecall")
      [ Y.FunctionCallE (Y.IdentifierE "gas") []
      , Y.IdentifierE "facet"
      , Y.LiteralE (Y.NumberLit 0)
      , Y.FunctionCallE (Y.IdentifierE "calldatasize") []
      , Y.LiteralE (Y.NumberLit 0)
      , Y.LiteralE (Y.NumberLit 0)
      ])
    , Y.ExpressionStmt $ Y.FunctionCallE (Y.IdentifierE "returndatacopy")
      [ Y.LiteralE (Y.NumberLit 0)
      , Y.LiteralE (Y.NumberLit 0)
      , Y.FunctionCallE (Y.IdentifierE "returndatasize") []
      ]
    , Y.SwitchStmt (Y.IdentifierE "result")
      [ ("0", Y.ExpressionStmt $ Y.FunctionCallE (Y.IdentifierE "revert")
        [ Y.LiteralE (Y.NumberLit 0)
        , Y.FunctionCallE (Y.IdentifierE "returndatasize") []
        ])
      , ("", Y.ExpressionStmt $ Y.FunctionCallE (Y.IdentifierE "return")
        [ Y.LiteralE (Y.NumberLit 0)
        , Y.FunctionCallE (Y.IdentifierE "returndatasize") []
        ])
      ]
    ]
  ]

transpileProxyReceive :: ContractBodyElem
transpileProxyReceive = FallbackDef $ FunctionDefinition
  { functionId = "receive"
  , S.functionVisibility = S.External
  , S.functionMutability = S.Mutable
  , functionPayablity = True
  , functionVirtualSpec = False
  , functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [FunctionArg unit S.Memory ""]
  , S.functionBody = [S.NoOpStmt]
  }

defaultProxyImports :: [S.Identifier]
defaultProxyImports = ["LibDiamond", "IDiamondCut"]

transpileFacet :: ([I.Import], I.Contract) -> Maybe S.Module
transpileFacet (is, f@I.FacetContract { facetName, proxyList }) =
  Just $ S.Module { moduleId=facetName, S.imports=(importIds =<< is) <> [proxyList], decls=transpileFacetContract f }
transpileFacet _ = Nothing

transpileFacetContract :: I.Contract -> [S.Declaration]
transpileFacetContract (FacetContract fId pId fns) = [ContractDef contractDef]
  where contractDef = ContractDefinition False fId [] (transpileFacetFn <$> fns)
transpileFacetContract _ = []

transpileFacetFn :: Function -> ContractBodyElem
transpileFacetFn (Function hd body) = FunctionDef $ FunctionDefinition
  { functionId = functionName hd
  , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility hd
  , S.functionMutability = transpileMutability $ I.functionMutability hd
  , functionPayablity = transpilePayability $ I.functionPayability hd
  , functionVirtualSpec = False
  , functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [FunctionArg unit S.Memory ""]
  , S.functionBody = [S.NoOpStmt]
  }

transpileType :: I.Type -> S.Type
transpileType = \case
  I.AddressT -> S.PrimitiveT S.AddressT
  I.BoolT -> S.PrimitiveT S.BoolT
  I.BytesT n -> S.PrimitiveT $ S.BytesT n
  I.BytesDynamicT -> S.PrimitiveT S.BytesDynamicT
  I.UIntT n -> S.PrimitiveT $ S.UintT n
  I.StringT -> S.PrimitiveT S.StringT
  I.UnitT -> S.PrimitiveT S.UnitT
  I.UserDefinedT id -> S.PrimitiveT $ S.UserDefinedT id
  I.StructT id -> S.PrimitiveT $ S.StructT id
  I.EnumT id -> S.PrimitiveT $ S.EnumT id
  I.ContractT id -> S.PrimitiveT $ S.ContractT id

transpileVisibility :: Maybe I.MemberVisibility -> S.Visibility
transpileVisibility = \case
  Just I.Public -> S.Public
  Just I.Internal -> S.Internal
  Just I.External -> S.External
  _ -> S.Private

transpileMutability :: I.Mutability -> S.Mutability
transpileMutability = \case
  I.Mutable -> S.Mutable
  I.View -> S.View

transpilePayability :: I.PayabilityKind -> Bool
transpilePayability = \case
  Payable -> True
  NonPayable -> False

transpileLocation :: I.MemoryLocation -> S.MemoryLocation
transpileLocation = \case
  I.Storage -> S.Storage
  I.Memory -> S.Memory

transpileValue :: I.Value -> S.Literal
transpileValue = \case
  AddressV v -> S.HexLit v
  BoolV b -> S.BooleanLit b
  BytesV v -> S.HexLit v
  UIntV n -> S.NumberLit n
  StringV v -> S.StringLit v

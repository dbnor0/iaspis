{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Proxy where
  
import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Yul.Grammar qualified as Y
import Iaspis.DeclUtils
import Transpile.Types qualified as T
import Transpile.Utils

transpileProxy :: ([I.Import], I.ProxyContract, [Facet]) -> T.Module
transpileProxy (is, p@I.ProxyContract { I.proxyName, I.facetList }, facets) =
  T.Module { T.moduleId=proxyName, T.imports=is', T.decls=proxyDecl }
  where proxyDecl = transpileProxyContract p facets
        is' = defaultProxyImports <> (I.importIds =<< is) <> facetList

transpileProxyContract :: I.ProxyContract -> [Facet] -> [T.Declaration]
transpileProxyContract (I.ProxyContract _ pId _ _) facets = [T.ContractDef contractDef]
  where contractDef = S.ContractDefinition False pId [] contractBody
        contractBody =
          [ transpileProxyConstructor facets
          , transpileProxyFallback
          , transpileProxyReceive
          ]

transpileProxyConstructor :: [Facet] -> S.ContractBodyElem
transpileProxyConstructor facets = S.ConstructorDef $ S.FunctionDefinition
  { S.functionId = "constructor"
  , S.functionVisibility = S.Public
  , S.functionMutability = S.Mutable
  , S.functionPayablity = True
  , S.functionVirtualSpec = False
  , S.functionOverrideSpec = False
  , S.functionArgs = proxyConstructorArgs $ fst <$> facets
  , S.functionReturnType = [S.FunctionArg unit S.Memory ""]
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

proxyFacetCutSelectors :: (Int, S.Identifier, I.Function) -> S.Statement
proxyFacetCutSelectors (idx, fId, I.Function hd _) =
  S.AssignmentStmt
    (S.SubscriptE (S.IdentifierE (fId <> "functionSelectors")) (S.LiteralE (S.NumberLit idx)))
    (S.IdentifierE $ fId <> "." <> I.functionName hd <> ".selector")

proxyConstructorArgs :: [S.Identifier] -> [S.FunctionArg]
proxyConstructorArgs fIds = S.FunctionArg address S.Memory "_contractOwner" : (facetAddress <$> fIds)
  where facetAddress fId = S.FunctionArg address S.Memory ("_" <> fId <> "Address")

transpileProxyFallback :: S.ContractBodyElem
transpileProxyFallback = S.FallbackDef $ S.FunctionDefinition
  { S.functionId = "fallback"
  , S.functionVisibility = S.External
  , S.functionMutability = S.Mutable
  , S.functionPayablity = True
  , S.functionVirtualSpec = False
  , S.functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [S.FunctionArg unit S.Memory ""]
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

transpileProxyReceive :: S.ContractBodyElem
transpileProxyReceive = S.FallbackDef $ S.FunctionDefinition
  { S.functionId = "receive"
  , S.functionVisibility = S.External
  , S.functionMutability = S.Mutable
  , S.functionPayablity = True
  , S.functionVirtualSpec = False
  , S.functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [S.FunctionArg unit S.Memory ""]
  , S.functionBody = [S.NoOpStmt]
  }

defaultProxyImports :: [S.Identifier]
defaultProxyImports = ["LibDiamond", "IDiamondCut"]


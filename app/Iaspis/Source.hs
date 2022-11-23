{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Iaspis.Source where

import Data.Fix
import Data.Text
import Prelude hiding (Enum)
import Data.Deriving (deriveShow1)


type Identifier = Text

data Type
  = AddressT 
  | BoolT
  | BytesT Int
  | BytesDynamicT
  | UIntT Int
  | StringT
  deriving stock Show

data Value
  = AddressV Text
  | BoolV Bool
  | BytesV Text
  | UIntV Int
  | StringV Text
  deriving stock Show

data BinaryOp 
  = AdditionOp
  | SubtractionOp
  | MultiplicationOp
  | DivisionOp
  | ModuloOp
  | ConjunctionOp
  | DisjunctionOp
  | EqualityOp
  | InequalityOp
  | LessThanOp
  | GreaterThanOp
  | LessThanEqualOp
  | GreaterThanEqualOp
  | LeftShiftOp
  | RightShiftOp
  | BitwiseConjunctionOp
  | BitwiseDisjunctionOp
  | BitwiseExclDisjunctionOp
  deriving stock Show

data UnaryOp
  = ArithmeticNegationOp
  | LogicalNegationOp
  | BitwiseNegationOp
  | IncrementOp
  | DecrementOp
  deriving stock Show

data ExpressionF a
  = LiteralE Value
  | IdentifierE Identifier
  | FunctionCallE Identifier [a]
  | UnaryE UnaryOp a
  | BinaryE BinaryOp a a
  
deriveShow1 ''ExpressionF

type Expression = Fix ExpressionF

data MemoryLocation 
  = Storage
  | Memory
  deriving stock (Eq, Show)

data ProxyMemberKind 
  = SharedProxyMember
  | UniqueProxyMember Identifier
  deriving stock (Eq, Show)

data MemberVisibility
  = Public
  | Private
  | Internal
  | External
  deriving stock (Eq, Show)

data PayabilityKind
  = Payable
  | NonPayable
  deriving stock (Eq, Show)

data Mutability
  = Mutable
  | View
  deriving stock (Eq, Show)

data Field = Field
  { fieldProxyKind :: Maybe ProxyMemberKind
  , fieldVisibility :: Maybe MemberVisibility
  , fieldMutability :: Mutability
  , fieldType :: Type
  , fieldLocation :: Maybe MemoryLocation
  , fieldName :: Identifier
  } deriving stock Show


data StatementF a
  = VarDeclStmt Field (Maybe (MemoryLocation, Expression))
  | AssignmentStmt Identifier MemoryLocation Expression 
  | ReturnStmt (Maybe Expression)
  | IfStmt Expression a (Maybe a)
  | BlockStmt [a]
  | BreakStmt
  | ContinueStmt
  | ExpressionStmt Expression
  deriving stock Show

deriveShow1 ''StatementF

type Statement = Fix StatementF

data FunctionHeader = FunctionHeader
  { functionVisibility :: MemberVisibility
  , functionPayability :: PayabilityKind
  , functionMutability :: Mutability
  , functionName :: Identifier
  , functionArgs :: [Field]
  , functionReturnType :: Maybe Type
  , overrideSpecifier :: Bool
  } deriving stock Show

data Function = Function
  { functionHeader :: FunctionHeader 
  , functionBody :: [Statement]
  } deriving stock Show

data ProxyKind 
  = ProxyOpen 
  | ProxyClosed
  deriving stock Show

data MemberDecl 
  = FieldDecl Field 
  | FunctionImpl Function
  deriving stock Show

newtype ModuleDecl = ModuleDecl Identifier
  deriving stock Show

newtype Declaration 
  = ContractDecl Contract 
  deriving stock Show

data Contract 
  = ImmutableContract 
  { contractName :: Identifier
  , contractDecls :: [MemberDecl] 
  }
  | ProxyContract 
  { proxyContractKind :: ProxyKind
  , proxyName :: Identifier
  , facetList :: [Identifier]
  , proxyDecls :: [Field] 
  }
  | FacetContract 
  { facetName :: Identifier
  , proxyList :: [Identifier]
  , facetDecls :: [Function] 
  }
  deriving stock Show

data Module = Module 
  { moduleDecl :: ModuleDecl
  , declarations :: [Declaration] 
  } deriving stock Show

{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Iaspis.Source where

import Data.Text
import Prelude hiding (Enum)


type Identifier = Text

data Type
  = AddressT 
  | BoolT
  | BytesT Int
  | BytesDynamicT
  | UIntT Int
  | StringT
  | UnitT
  deriving stock (Eq, Show)

data Value
  = AddressV Text
  | BoolV Bool
  | BytesV Text
  | UIntV Int
  | StringV Text
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

data UnaryOp
  = ArithmeticNegationOp
  | LogicalNegationOp
  | BitwiseNegationOp
  | IncrementOp
  | DecrementOp
  deriving stock (Eq, Show)

data Expression
  = LiteralE Value
  | IdentifierE Identifier
  | FunctionCallE Identifier [Expression]
  | UnaryE UnaryOp Expression
  | BinaryE BinaryOp Expression Expression
  deriving stock (Eq, Show)
  
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
  , fieldLocation :: MemoryLocation
  , fieldName :: Identifier
  } deriving stock (Eq, Show)

data Statement
  = VarDeclStmt Field MemoryLocation Expression
  | AssignmentStmt Identifier MemoryLocation Expression 
  | ReturnStmt (Maybe Expression)
  | IfStmt Expression Statement (Maybe Statement)
  | BlockStmt [Statement]
  | BreakStmt
  | ContinueStmt
  | ExpressionStmt Expression
  deriving stock (Eq, Show)

data FunctionHeader = FunctionHeader
  { functionVisibility :: MemberVisibility
  , functionPayability :: PayabilityKind
  , functionMutability :: Mutability
  , functionName :: Identifier
  , functionArgs :: [Field]
  , functionReturnType :: Type
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
  , contractFields :: [Field]
  , contractFns :: [Function] 
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
  { moduleDecl :: Identifier
  , declarations :: [Declaration] 
  } deriving stock Show

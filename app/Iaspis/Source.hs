module Iaspis.Source where

import Data.Text
import Prelude hiding (Enum)
import Text.Megaparsec (State)


type Identifier = Text

newtype ModuleDecl = ModuleDecl Identifier
  deriving (Eq, Show)

newtype Import = Import Identifier
  deriving (Eq, Show)

data Declaration 
  = ContractDecl Contract 
  | StructDecl Struct
  | EnumDecl Enum
  deriving (Eq, Show)

data Contract 
  = ImmutableContract 
  { contractName :: Identifier
  , inheritanceList :: [Identifier]
  , contractDecls :: [MemberDecl] 
  }
  | ProxyContract 
  { proxyContractKind :: ProxyKind
  , proxyName :: Identifier
  , facetList :: [Identifier]
  , proxyDecls :: [MemberDecl] 
  }
  | FacetContract 
  { facetName :: Identifier
  , proxyList :: [Identifier]
  , facetDecls :: [MemberDecl] 
  }
  deriving (Eq, Show)

data ProxyKind 
  = ProxyOpen 
  | ProxyClosed
  deriving (Eq, Show)

data MemberDecl
  = FieldDecl 
  { fieldProxyKind :: Maybe ProxyMemberKind
  , fieldVisibility :: Maybe MemberVisibility
  , fieldModifiers :: [FieldModifier]
  , fieldType :: Type
  , fieldName :: Identifier
  }
  | FunctionDecl 
  { functionVisibility :: MemberVisibility
  , functionPayability :: PayabilityKind
  , functionKind :: FunctionKind
  , functionName :: Identifier
  , functionSig :: FunctionSignature
  , functionBody :: [Statement]
  } deriving (Eq, Show)

data ProxyMemberKind 
  = SharedProxyMember
  | UniqueProxyMember Identifier
  deriving (Eq, Show)

data MemberVisibility
  = Public
  | Private
  | Internal
  | External
  deriving (Eq, Show)

data FieldModifier = ConstMod
  deriving (Eq, Show)

data PayabilityKind
  = Payable
  | NonPayable
  deriving (Eq, Show)

data FunctionKind
  = Function
  | Procedure
  deriving (Eq, Show)

data Arg = Arg 
  { argType :: Type
  , argLocation :: Maybe MemoryLocation
  , argName :: Identifier 
  } deriving (Eq, Show)

data FunctionSignature = 
  FunctionSignature { inputArgs :: [Arg], returnType :: Type }
  deriving (Eq, Show)

data MemoryLocation 
  = Storage
  | Memory
  deriving (Eq, Show)

data Statement
  = VarDeclStmt Arg (Maybe (MemoryLocation, Expression))
  | AssignmentStmt Identifier MemoryLocation Expression 
  | ReturnStmt Expression
  | IfStmt Expression Statement (Maybe Statement)
  | WhileStmt Expression Statement
  | ForStmt (Maybe Statement) (Maybe Expression) (Maybe Expression) Statement
  | ForEachStmt Identifier Expression Statement
  | BlockStmt [Statement]
  | BreakStmt
  | ContinueStmt
  | ExpressionStmt Expression
  deriving (Eq, Show)

data Expression 
  = LiteralE Value
  | IdentifierE Identifier
  | SubscriptE Expression Expression
  | MemberAccessE Expression Identifier
  | FunctionCallE Identifier [Expression]
  | UnaryE UnaryOp Expression
  | BinaryE BinaryOp Expression Expression
  | TernaryE Expression Expression Expression
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data UnaryOp
  = ArithmeticNegationOp
  | LogicalNegationOp
  | BitwiseNegationOp
  | IncrementOp
  | DecrementOp
  deriving (Eq, Show)

data Struct = Struct 
  { structName :: Identifier, structFields :: [Arg] } 
  deriving (Eq, Show)

data Enum = Enum 
  { enumName :: Identifier, enumFields :: [Identifier] }
  deriving (Eq, Show)

data Module = Module 
  { moduleDecl :: ModuleDecl
  , imports :: [Import]
  , declarations :: [Declaration] 
  } deriving (Eq, Show)

data Type
  = AddressT 
  | BoolT
  | BytesT Int
  | BytesDynamicT
  | UIntT Int
  | StringT
  | UserDefinedT Identifier
  | ArrayT Type [Maybe Int]
  | MappingT Type Type
  deriving (Eq, Show)

data Value
  = AddressV Text
  | BoolV Bool
  | BytesV Text
  | UIntV Int
  | StringV Text
  | StructV [(Identifier, Expression)]
  | ArrayV [Expression]
  | EnumV Identifier
  deriving (Eq, Show)

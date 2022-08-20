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
  | InterfaceDecl Interface
  | StructDecl Struct
  | EnumDecl Enum
  deriving (Eq, Show)

data Contract 
  = ImmutableContract 
  { abstractSpeicifer :: Bool
  , contractName :: Identifier
  , cInheritanceList :: [Identifier]
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
  deriving (Eq, Show)

data Interface = Interface
  { interfaceName :: Identifier
  , iInheritanceList :: [Identifier]
  , interfaceDecls :: [FunctionHeader]
  } deriving (Eq, Show)

data ProxyKind 
  = ProxyOpen 
  | ProxyClosed
  deriving (Eq, Show)

data MemberDecl 
  = FieldDecl Field 
  | FunctionDecl FunctionHeader
  | FunctionImpl Function
  deriving (Eq, Show)

data Field = Field
  { fieldProxyKind :: Maybe ProxyMemberKind
  , fieldVisibility :: Maybe MemberVisibility
  , fieldMutability :: Mutability
  , fieldType :: Type
  , fieldLocation :: Maybe MemoryLocation
  , fieldName :: Identifier
  } deriving (Eq, Show)

data FunctionHeader = FunctionHeader
  { functionVisibility :: MemberVisibility
  , functionPayability :: PayabilityKind
  , functionMutability :: Mutability
  , functionName :: Identifier
  , functionArgs :: [Field]
  , functionReturnType :: Maybe Type
  , overrideSpecifier :: Bool
  } deriving (Eq, Show)

data Function = Function
  { functionHeader :: FunctionHeader 
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

data Mutability
  = Mutable
  | View
  deriving (Eq, Show)

data MemoryLocation 
  = Storage
  | Memory
  deriving (Eq, Show)

data Statement
  = VarDeclStmt Field (Maybe (MemoryLocation, Expression))
  | AssignmentStmt Identifier MemoryLocation Expression 
  | ReturnStmt (Maybe Expression)
  | IfStmt Expression Statement (Maybe Statement)
  | WhileStmt Expression Statement
  | ForStmt (Maybe Statement) (Maybe Expression) (Maybe Expression) Statement
  | ForEachStmt Identifier Expression Statement
  | BlockStmt [Statement]
  | BreakStmt
  | ContinueStmt
  | RequireStmt Expression (Maybe Expression)
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
  { structName :: Identifier, structFields :: [Field] } 
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
  | ArrayV [Expression]
  | EnumV Identifier
  deriving (Eq, Show)

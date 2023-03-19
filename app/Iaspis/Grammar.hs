{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveGeneric #-}

module Iaspis.Grammar where

import Data.Text ( Text )
import Prelude hiding (Enum)
import GHC.Generics
import Data.Aeson hiding (Value)


type Identifier = Text

data Type
  = AddressT
  | BoolT
  | BytesT Int
  | UIntT
  | StringT
  | UnitT
  | UserDefinedT Identifier
  | StructT Struct
  | EnumT Enum
  | ContractT Identifier
  deriving stock (Eq, Show, Generic)

instance ToJSON Type where

data Value
  = AddressV Text
  | BoolV Bool
  | BytesV Text
  | UIntV Int
  | StringV Text
  | StructV StructValue
  deriving stock (Eq, Show, Generic)

instance ToJSON Value where

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
  deriving stock (Eq, Show, Generic)

instance ToJSON BinaryOp where

data UnaryOp
  = ArithmeticNegationOp
  | LogicalNegationOp
  | BitwiseNegationOp
  | IncrementOp
  | DecrementOp
  deriving stock (Eq, Show, Generic)

instance ToJSON UnaryOp where

data Expression
  = LiteralE Value
  | IdentifierE Identifier
  | MemberAccessE Expression Identifier
  | FunctionCallE Identifier [Expression]
  | InstantiationE Identifier [Expression]
  | UnaryE UnaryOp Expression
  | BinaryE BinaryOp Expression Expression
  deriving stock (Eq, Show, Generic)

instance ToJSON Expression where
  
data MemoryLocation 
  = Storage
  | Memory
  deriving stock (Eq, Show, Generic)

instance ToJSON MemoryLocation where

data ProxyMemberKind 
  = SharedProxyMember
  | UniqueProxyMember Identifier
  deriving stock (Eq, Show, Generic)

instance ToJSON ProxyMemberKind where

data MemberVisibility
  = Public
  | Private
  | Internal
  | External
  deriving stock (Eq, Show, Generic)

instance ToJSON MemberVisibility where

data PayabilityKind
  = Payable
  | NonPayable
  deriving stock (Eq, Show, Generic)

instance ToJSON PayabilityKind where

data Mutability
  = Mutable
  | View
  deriving stock (Eq, Show, Generic)

instance ToJSON Mutability where

data FunctionArg = FunctionArg
  { argType :: Type
  , argName :: Identifier
  , argLocation :: MemoryLocation
  } deriving stock (Eq, Show, Generic)

instance ToJSON FunctionArg where

data DeclArg = DeclArg
  { declMutability :: Mutability
  , declType :: Type
  , declName :: Identifier
  , declLocation :: MemoryLocation
  } deriving stock (Eq, Show, Generic)

instance ToJSON DeclArg where

data Field = Field
  { fieldProxyKind :: Maybe ProxyMemberKind
  , fieldVisibility :: Maybe MemberVisibility
  , fieldMutability :: Mutability
  , fieldType :: Type
  , fieldLocation :: MemoryLocation
  , fieldName :: Identifier
  , fieldInitializer :: Maybe Expression
  } deriving stock (Eq, Show, Generic)

instance ToJSON Field where

data Statement
  = VarDeclStmt DeclArg MemoryLocation Expression
  | AssignmentStmt Expression MemoryLocation Expression 
  | ReturnStmt (Maybe Expression)
  | IfStmt Expression Statement (Maybe Statement)
  | WhileStmt Expression Statement
  | BlockStmt [Statement]
  | BreakStmt
  | ContinueStmt
  | ExpressionStmt Expression
  deriving stock (Eq, Show, Generic)

instance ToJSON Statement where


data FunctionHeader = FunctionHeader
  { functionVisibility :: MemberVisibility
  , functionPayability :: PayabilityKind
  , functionMutability :: Mutability
  , functionName :: Identifier
  , functionArgs :: [FunctionArg]
  , functionReturnType :: FunctionArg
  , overrideSpecifier :: Bool
  } deriving stock (Show, Generic)

instance ToJSON FunctionHeader where

data Function = Function
  { functionHeader :: FunctionHeader 
  , functionBody :: [Statement]
  } deriving stock (Show, Generic)

instance ToJSON Function where

data ProxyKind 
  = ProxyOpen 
  | ProxyClosed
  deriving stock (Show, Generic)

instance ToJSON ProxyKind where


data MemberDecl 
  = FieldDecl Field 
  | FunctionImpl Function
  deriving stock Show

newtype ModuleDecl = ModuleDecl Identifier
  deriving stock Show

data Declaration 
  = ContractDecl ImmutableContract
  | ProxyDecl ProxyContract
  | FacetDecl FacetContract
  | StructDecl Struct
  | EnumDecl Enum
  deriving stock (Show, Generic)

instance ToJSON Declaration where

data ImmutableContract = ImmutableContract 
  { contractName :: Identifier
  , contractFields :: [Field]
  , contractFns :: [Function] 
  } deriving stock (Show, Generic)

instance ToJSON ImmutableContract where

data ProxyContract = ProxyContract 
  { proxyContractKind :: ProxyKind
  , proxyName :: Identifier
  , facetList :: [Identifier]
  , proxyDecls :: [Field] 
  } deriving stock (Show, Generic)

instance ToJSON ProxyContract where

data FacetContract = FacetContract 
  { facetName :: Identifier
  , proxyList :: Identifier
  , facetDecls :: [Function] 
  } deriving stock (Show, Generic)
  
instance ToJSON FacetContract where

data Struct = Struct
  { structName :: Identifier
  , structFields :: [StructField]
  } deriving stock (Eq, Show, Generic)

instance ToJSON Struct where

data StructField = StructField
  { structFieldType :: Type
  , structFieldName :: Identifier
  } deriving stock (Eq, Show, Generic)

instance ToJSON StructField where

data StructValue = StructValue
  { structValueName :: Identifier
  , structValueMembers :: [StructValueMember]
  } deriving stock (Eq, Show, Generic)

instance ToJSON StructValue where

data StructValueMember = StructValueMember
  { structMemberValueName :: Identifier
  , structMemberValueExpr :: Expression
  } deriving stock (Eq, Show, Generic)

instance ToJSON StructValueMember where

data Enum = Enum
  { enumName :: Identifier
  , enumFields :: [Identifier]
  } deriving stock (Eq, Show, Generic)

instance ToJSON Enum where

data Import = Import
  { importIds :: [Identifier]
  , importModule :: Identifier
  } deriving stock (Eq, Show, Generic)

instance ToJSON Import where

data Module = Module 
  { moduleDecl :: Identifier
  , imports :: [Import]
  , declarations :: [Declaration] 
  } deriving stock (Show, Generic)

instance ToJSON Module where

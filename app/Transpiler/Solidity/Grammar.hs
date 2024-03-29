{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Solidity.Grammar where

import Data.Text as T
import Transpiler.Yul.Grammar qualified as Yul


type Identifier = T.Text
type ImportPath = T.Text

newtype Import = Import ImportPath
  deriving stock (Eq, Show)

data ContractDefinition = ContractDefinition
  { contractAbstractSpec :: Bool
  , contractId :: Identifier
  , contractInheritanceList :: [Identifier]
  , contractBody :: [ContractBodyElem]
  } deriving stock (Eq, Show)

data InterfaceDefinition = InterfaceDefinition
  { interfaceId :: Identifier
  , interfaceInheritanceList :: [Identifier]
  , interfaceBody :: [ContractBodyElem]
  } deriving stock (Eq, Show)

data LibraryDefinition = LibraryDefinition
  { libraryId :: Identifier
  , libraryBody :: [ContractBodyElem]
  } deriving stock (Eq, Show)

data StructDefinition = StructDefinition
  { structId :: Identifier
  , structMembers :: [StructMember]
  } deriving stock (Eq, Show)

data StructMember = StructMember
  { structMemberType :: Type
  , structMemberId :: Identifier
  } deriving stock (Eq, Show)

data EnumDefinition = EnumDefinition
  { enumId :: Identifier
  , enumMembers :: [Identifier]
  } deriving stock (Eq, Show)

data ContractBodyElem
  = ConstructorDef FunctionDefinition
  | FunctionDef FunctionDefinition
  | FallbackDef FunctionDefinition
  | ReceiveDef FunctionDefinition
  | StructDef StructDefinition
  | EnumDef EnumDefinition
  | StateVarDecl StateVarDeclaration
  deriving stock (Eq, Show)

data FunctionDefinition = FunctionDefinition
  { functionId :: Identifier
  , functionVisibility :: Visibility
  , functionMutability :: Mutability
  , functionPayablity :: Bool
  , functionVirtualSpec :: Bool
  , functionOverrideSpec :: Bool
  , functionArgs :: [FunctionArg]
  , functionReturnType :: [FunctionArg]
  , functionBody :: [Statement]
  } deriving stock (Eq, Show)

data FunctionArg = FunctionArg
  { functionArgType :: Type
  , functionArgId :: Identifier
  } deriving stock (Eq, Show)

data StateVarDeclaration = StateVarDeclaration
  { stateVarType :: Type
  , stateVarVisibility :: Visibility
  , stateVarModifier :: Maybe StateVarModifier
  , stateVarId :: Identifier
  , stateVarInitializer :: Maybe Expression
  } deriving stock (Eq, Show)

data Visibility
  = Public
  | Private
  | Internal
  | External
  deriving stock (Eq, Show)

data Mutability 
  = Mutable
  | View
  | Pure
  deriving stock (Eq, Show)

data StateVarModifier
  = Constant
  | Immutable
  deriving stock (Eq, Show)

data MemoryLocation
  = Storage
  | Memory
  | Calldata
  deriving stock (Eq, Show)

data Type
  = PrimitiveT PrimitiveType
  | MappingT MappingType
  | ArrayT ArrayType
  deriving stock (Eq, Show)

data PrimitiveType
  = AddressT
  | PayableAddressT
  | BoolT
  | StringT (Maybe MemoryLocation)
  | BytesT Int
  | IntT Int
  | UintT Int
  | BytesDynamicT (Maybe MemoryLocation)
  | UnitT
  | UserDefinedT Identifier (Maybe MemoryLocation)
  | StructT Identifier (Maybe MemoryLocation)
  | EnumT Identifier
  | ContractT Identifier
  deriving stock (Eq, Show)

data MappingType = MappingType
  { mappingKeyType :: Type
  , mappingValueType :: Type
  } deriving stock (Eq, Show)

data ArrayType = ArrayType
  { arrayType :: Type
  , arraySize :: [Maybe Int]
  , arrayLocation :: Maybe MemoryLocation
  } deriving stock (Eq, Show)

data Expression
  = LiteralE Literal
  | IdentifierE Identifier
  | InlineArrayE [Expression]
  | MemberAccessE Expression Expression
  | SubscriptE Expression Expression
  | FunctionCallE Expression [Expression]
  | InstantiationE Expression [Expression]
  | ArrayInstantiationE Identifier Expression
  | CastE Type Expression
  | BinaryE BinaryOp Expression Expression
  | UnaryE UnaryOp Expression
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

data Statement
  = BlockStmt [Statement]
  | VarDeclStmt FunctionArg (Maybe Expression)
  | AssignmentStmt Expression Expression
  | ExpressionStmt Expression
  | IfStmt Expression Statement (Maybe Statement)
  | ForStmt Statement Statement Expression Statement
  | WhileStmt Expression Statement
  | ContinueStmt
  | BreakStmt
  | ReturnStmt (Maybe Expression)
  | RevertStmt Expression
  | AssemblyStmt Yul.Statement
  | NoOpStmt
  deriving stock (Eq, Show)
  
data Literal
  = StringLit T.Text
  | NumberLit Int 
  | BooleanLit Bool
  | HexLit T.Text
  | EnumLit T.Text T.Text
  | StructLit Identifier [(Identifier, Expression)]
  | ArrayLit [Expression]
  deriving stock (Eq, Show)
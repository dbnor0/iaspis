module Source where

import Prelude hiding (Enum)
import Data.Text

type Identifier = Text

newtype Import = Import 
    { getPath :: Text } 
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
    { fieldProxyKind :: ProxyMemberKind
    , fieldVisibility :: MemberVisibility
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
    = VarDeclStmt Arg (Maybe Expression)
    | MemoryAssignmentStmt Identifier Expression 
    | StorageAssignmentStmt Identifier Expression
    | ReturnStmt Expression
    | IfStmt Expression Statement (Maybe Statement)
    | BlockStmt [Statement]
    deriving (Eq, Show)

data Expression = Expression
    deriving (Eq, Show)

data Struct = Struct 
    { structName :: Identifier, structFields :: [Arg] } 
    deriving (Eq, Show)

data Enum = Enum 
    { enumName :: Identifier, enumFields :: [Identifier] }
    deriving (Eq, Show)

data Source = Source 
    { imports :: [Import]
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
    deriving Eq

instance Show Type where
    show AddressT = "address"
    show BoolT = "bool"
    show (BytesT n) = "bytes" ++ show n
    show BytesDynamicT = "bytes"
    show (UIntT n) = "uint" ++ show n
    show StringT = "string"
    show (UserDefinedT name) = unpack name
    show (ArrayT t dimensions) = show t ++ Prelude.concatMap showDim  dimensions
        where showDim n = "[" ++ maybe "" show n ++ "]"
    show (MappingT k v) = "mapping (" ++ show k ++ " => " ++ show v ++ ")"
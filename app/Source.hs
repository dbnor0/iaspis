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
    }
    deriving (Eq, Show)

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
    FunctionSignature { inputArgs :: [Arg], returnType :: Type}
    deriving (Eq, Show)

data MemoryLocation 
    = Storage
    | Memory
    deriving (Eq, Show)

data Statement
    = VarDeclStatement Type Identifier (Maybe Expression)
    | MemoryAssignment Identifier Expression
    | StorageAssignment Identifier Expression

data Expression

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
    | DynamicBytesT
    | UIntT Int
    | StringT 
    deriving (Eq, Show)
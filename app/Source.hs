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
    = ImmutableContract { contractName :: Identifier, inheritanceList :: [Identifier] }
    | ProxyContract { proxyKind :: ProxyKind, proxyName :: Identifier, facetList :: [Identifier] }
    | FacetContract { facetName :: Identifier, proxyList :: [Identifier] }
    deriving (Eq, Show)

data ProxyKind 
    = ProxyOpen 
    | ProxyClosed
    deriving (Eq, Show)

newtype Struct = Struct 
    { structName :: Identifier } 
    deriving (Eq, Show)

newtype Enum = Enum 
    { enumName :: Identifier }
    deriving (Eq, Show)

data Source = Source 
    { imports :: [Import]
    , declarations :: [Declaration] 
    } deriving (Eq, Show)

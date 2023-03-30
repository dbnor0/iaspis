{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Iaspis.DeclUtils where
import Iaspis.Grammar
import Iaspis.Grammar qualified as I (facetDecls)
import Data.Maybe
import Prelude hiding (Enum)


type Facet = (Identifier, [Function])

contractDecls :: Module -> [([Import], ImmutableContract)]
contractDecls Module{ imports, declarations } = mapMaybe getContract declarations
  where getContract (ContractDecl c@ImmutableContract{}) = Just (imports, c)
        getContract _ = Nothing

proxyDecls :: [FacetContract] -> Module -> [([Import], ProxyContract, [Facet])]
proxyDecls fs Module{ imports, declarations } = mapMaybe getContract declarations
  where getContract (ProxyDecl p@ProxyContract{ facetList }) = Just (imports, p, getFacet =<< facetList)
          where getFacet fId = mapMaybe (filterFn fId) fs
                filterFn fId FacetContract{ facetName, I.facetDecls }
                  | facetName == fId = Just (facetName, facetDecls)
                  | otherwise = Nothing
        getContract _ = Nothing

facetDecls :: Module -> [([Import], Module, FacetContract)]
facetDecls m@Module{ imports, declarations } = mapMaybe getContract declarations
  where getContract (FacetDecl f@FacetContract{}) = Just (imports, m, f)
        getContract _ = Nothing

structDecls :: Module -> [Struct]
structDecls Module{ declarations } = mapMaybe filterDecl declarations
  where filterDecl (StructDecl s) = Just s
        filterDecl _ = Nothing

enumDecls :: Module -> [Enum]
enumDecls Module{ declarations } = mapMaybe filterDecl declarations
  where filterDecl (EnumDecl e) = Just e
        filterDecl _ = Nothing

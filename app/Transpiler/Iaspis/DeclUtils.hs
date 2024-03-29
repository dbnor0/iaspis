{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Iaspis.DeclUtils where

import Transpiler.Iaspis.Grammar
import Transpiler.Iaspis.Grammar qualified as I (facetDecls)
import Data.Maybe
import Prelude hiding (Enum)


type Facet = (Identifier, [Function])

contractDecls :: Module -> [([Import], Module, ImmutableContract)]
contractDecls m@Module{ imports, declarations } = mapMaybe getContract declarations
  where getContract (ContractDecl c@ImmutableContract{}) = Just (imports, m, c)
        getContract _ = Nothing

proxyDecls :: [FacetContract] -> Module -> [([Import], ProxyContract, Module, [Facet])]
proxyDecls fs m@Module{ imports, declarations } = mapMaybe getContract declarations
  where getContract (ProxyDecl p@ProxyContract{ facetList }) = Just (imports, p, m, getFacet =<< facetList)
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

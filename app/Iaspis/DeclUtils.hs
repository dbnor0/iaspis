{-# LANGUAGE NamedFieldPuns #-}

module Iaspis.DeclUtils where
import Iaspis.Grammar as I
import Data.Maybe


type Facet = (Identifier, [Function])

contractDecls :: Module -> [([Import], Contract)]
contractDecls Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl c@ImmutableContract{}) = Just (imports, c)
        getContract _ = Nothing

proxyDecls :: [Contract] -> Module -> [([Import], Contract, [Facet])]
proxyDecls fs Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl p@ProxyContract{ facetList }) = Just (imports, p, getFacet =<< facetList)
          where getFacet fId = catMaybes $ filterFn fId <$> fs
                filterFn fId FacetContract{ facetName, I.facetDecls }
                  | facetName == fId = Just (facetName, facetDecls)
                  | otherwise = Nothing
                filterFn _ _ = Nothing
        getContract _ = Nothing

facetDecls :: Module -> [([Import], Contract)]
facetDecls Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl f@FacetContract{}) = Just (imports, f)
        getContract _ = Nothing

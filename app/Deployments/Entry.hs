{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Deployments.Entry where

import Transpiler.Iaspis.Grammar
import Transpiler.Analysis.Environment
import Lens.Micro.Platform
import Data.Foldable
import Data.Text qualified as  T


genDeployments :: BuildEnv -> IO ()
genDeployments env = do
  traverse_ writeContractDeployment (env ^. contracts)
  traverse_ writeProxyDeployment (env ^. proxies)

writeContractDeployment :: ContractEntry -> IO ()
writeContractDeployment c@(ContractEntry id _ _ _) =
  writeFile (fileName id) (T.unpack $ genContractDeployment c)

genContractDeployment :: ContractEntry -> T.Text
genContractDeployment (ContractEntry id _ _ _) =
  cImport <> cExport
  where cImport = "const " <> id <> " = artifacts.require(\"" <> id <> "\");\n"
        cExport =
          "module.exports = async (deployer, network, accounts) => {\n"
          <> "  await deployer.deploy(" <> id <> ");\n"
          <> "}\n"

writeProxyDeployment :: ProxyEntry -> IO ()
writeProxyDeployment p =
  writeFile (fileName (p ^. proxyId)) (T.unpack $ genProxyDeploymentWithFacet p)

genProxyDeploymentWithFacet :: ProxyEntry -> T.Text
genProxyDeploymentWithFacet p@ProxyEntry{ _proxyId, _proxyFacetList } =
  genProxyDeploymentImports _proxyId _proxyFacetList
  <> genProxyDeploymentExport p

genProxyDeploymentExport :: ProxyEntry -> T.Text
genProxyDeploymentExport ProxyEntry{ _proxyId, _proxyFacetList } = 
  "module.exports = async (deployer, network, accounts) => {\n"
  <> genStorageLibDeployment
  <> genDefaultFacetDeployment
  <> T.concat (genCustomFacetDeployment <$> _proxyFacetList)
  <> genProxyDeployment _proxyId _proxyFacetList
  <> "}\n"

genProxyDeploymentImports :: Identifier -> [Identifier] -> T.Text
genProxyDeploymentImports pId fIds =
  "const DiamondCutFacet = artifacts.require(\"DiamondCutFacet\");\n"
  <> "const DiamondLoupeFacet = artifacts.require(\"DiamondLoupeFacet\");\n"
  <> "const StorageStructs = artifacts.require(\"StorageStructs\");\n"
  <> T.concat (genCustomImport <$> pId : fIds)

genCustomImport :: Identifier -> T.Text
genCustomImport id = "const " <> id <> " = artifacts.require(\"" <> id <> "\");\n"

genStorageLibDeployment :: T.Text
genStorageLibDeployment =
  "  await deployer.deploy(StorageStructs);\n"
  <> "  const storageLib = await StorageStructs.deployed();\n";

genDefaultFacetDeployment :: T.Text
genDefaultFacetDeployment =
  "  await deployer.deploy(DiamondCutFacet);\n"
  <> "  const diamondCutFacet = await DiamondCutFacet.deployed();\n"
  <> "  await deployer.deploy(DiamondLoupeFacet);\n"
  <> "  const diamondLoupeFacet = await DiamondLoupeFacet.deployed();\n"

genCustomFacetDeployment :: Identifier -> T.Text
genCustomFacetDeployment id =
  "  await deployer.link(StorageStructs, " <> id <> ");\n"
  <> "  await deployer.deploy(" <> id <> ");\n"
  <> "  const " <> id <> "Facet = await " <> id <> ".deployed();\n"

genProxyDeployment :: Identifier -> [Identifier] -> T.Text
genProxyDeployment p fs = 
  "  await deployer.deploy(\n"
  <> "    " <> p <> ",\n"
  <> "    accounts[0],\n"
  <> "    diamondCutFacet.address,\n"
  <> "    diamondLoupeFacet.address,\n"
  <> T.concat (facetArg <$> fs)
  <> "  );\n"
  where facetArg id = "    " <> id <> "Facet.address,\n"

fileName :: Identifier -> FilePath
fileName id = "scripts/migrations/" <> T.unpack id <> "_deployment.js"
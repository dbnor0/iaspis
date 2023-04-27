{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpiler.Utils.SolContracts where

import  Data.Text qualified as T

rawContracts :: [(FilePath, T.Text)]
rawContracts =
  [ ("DiamondCutFacet", diamondCutFacet)
  , ("DiamondLoupeFacet", diamondLoupeFacet)
  , ("IDiamondCut", iDiamondCut)
  , ("IDiamondLoupe", iDiamondLoupe)
  , ("IERC165", ierc165)
  , ("LibDiamond", libDiamond)
  ]

diamondCutFacet :: T.Text
diamondCutFacet =
  " \
  \ // SPDX-License-Identifier: MIT \n\
  \ pragma solidity ^0.8.0; \n\
  \ \n\
  \ /******************************************************************************\ \n\
  \ * Author: Nick Mudge <nick@perfectabstractions.com> (https://twitter.com/mudgen) \n\
  \ * EIP-2535 Diamonds: https://eips.ethereum.org/EIPS/eip-2535 \n\
  \ /******************************************************************************/ \n\
  \ \n\
  \ import { IDiamondCut } from \"./IDiamondCut.sol\"; \n\
  \ import { LibDiamond } from \"./LibDiamond.sol\"; \n\
  \ \n\
  \ // Remember to add the loupe functions from DiamondLoupeFacet to the diamond. \n\
  \ // The loupe functions are required by the EIP2535 Diamonds standard \n\
  \ \n\
  \ contract DiamondCutFacet is IDiamondCut { \n\
  \     /// @notice Add/replace/remove any number of functions and optionally execute \n\
  \     ///         a function with delegatecall \n\
  \     /// @param _diamondCut Contains the facet addresses and function selectors \n\
  \     /// @param _init The address of the contract or facet to execute _calldata \n\
  \     /// @param _calldata A function call, including function selector and arguments \n\
  \     ///                  _calldata is executed with delegatecall on _init \n\
  \     function diamondCut( \n\
  \         FacetCut[] calldata _diamondCut, \n\
  \         address _init, \n\
  \         bytes calldata _calldata \n\
  \     ) external override { \n\
  \         LibDiamond.enforceIsContractOwner(); \n\
  \         LibDiamond.diamondCut(_diamondCut, _init, _calldata); \n\
  \     } \n\
  \ } \n\ 
  \"

diamondLoupeFacet :: T.Text
diamondLoupeFacet =
  " \
  \ // SPDX-License-Identifier: MIT \n\
  \ pragma solidity ^0.8.0; \n\
  \ /******************************************************************************\ \n\
  \ * Author: Nick Mudge <nick@perfectabstractions.com> (https://twitter.com/mudgen) \n\
  \ * EIP-2535 Diamonds: https://eips.ethereum.org/EIPS/eip-2535 \n\
  \ /******************************************************************************/ \n\
  \ \n\
  \ import { LibDiamond } from  \"./LibDiamond.sol\"; \n\
  \ import { IDiamondLoupe } from \"./IDiamondLoupe.sol\"; \n\ 
  \ import { IERC165 } from \"./IERC165.sol\"; \n\
  \ \n\
  \ // The functions in DiamondLoupeFacet MUST be added to a diamond. \n\
  \ // The EIP-2535 Diamond standard requires these functions. \n\
  \  \n\
  \ contract DiamondLoupeFacet is IDiamondLoupe, IERC165 { \n\
  \     // Diamond Loupe Functions \n\
  \     //////////////////////////////////////////////////////////////////// \n\
  \     /// These functions are expected to be called frequently by tools. \n\
  \     // \n\
  \     // struct Facet { \n\
  \     //     address facetAddress; \n\
  \     //     bytes4[] functionSelectors; \n\
  \     // } \n\
  \  \n\
  \     /// @notice Gets all facets and their selectors. \n\
  \     /// @return facets_ Facet \n\
  \     function facets() external override view returns (Facet[] memory facets_) { \n\
  \         LibDiamond.DiamondStorage storage ds = LibDiamond.diamondStorage(); \n\ 
  \         uint256 numFacets = ds.facetAddresses.length; \n\
  \         facets_ = new Facet[](numFacets); \n\
  \         for (uint256 i; i < numFacets; i++) { \n\
  \             address facetAddress_ = ds.facetAddresses[i]; \n\
  \             facets_[i].facetAddress = facetAddress_; \n\
  \             facets_[i].functionSelectors = ds.facetFunctionSelectors[facetAddress_].functionSelectors; \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     /// @notice Gets all the function selectors provided by a facet. \n\
  \     /// @param _facet The facet address. \n\
  \     /// @return facetFunctionSelectors_ \n\
  \     function facetFunctionSelectors(address _facet) external override view returns (bytes4[] memory facetFunctionSelectors_) { \n\
  \         LibDiamond.DiamondStorage storage ds = LibDiamond.diamondStorage(); \n\
  \         facetFunctionSelectors_ = ds.facetFunctionSelectors[_facet].functionSelectors; \n\
  \     } \n\
  \ \n\
  \     /// @notice Get all the facet addresses used by a diamond. \n\ 
  \     /// @return facetAddresses_ \n\
  \     function facetAddresses() external override view returns (address[] memory facetAddresses_) { \n\
  \         LibDiamond.DiamondStorage storage ds = LibDiamond.diamondStorage(); \n\
  \         facetAddresses_ = ds.facetAddresses; \n\
  \     } \n\
  \  \n\
  \     /// @notice Gets the facet that supports the given selector. \n\ 
  \     /// @dev If facet is not found return address(0). \n\
  \     /// @param _functionSelector The function selector. \n\
  \     /// @return facetAddress_ The facet address. \n\
  \     function facetAddress(bytes4 _functionSelector) external override view returns (address facetAddress_) { \n\
  \         LibDiamond.DiamondStorage storage ds = LibDiamond.diamondStorage(); \n\
  \         facetAddress_ = ds.selectorToFacetAndPosition[_functionSelector].facetAddress; \n\
  \     } \n\
  \ \n\
  \     // This implements ERC-165. \n\
  \     function supportsInterface(bytes4 _interfaceId) external override view returns (bool) { \n\
  \         LibDiamond.DiamondStorage storage ds = LibDiamond.diamondStorage(); \n\
  \         return ds.supportedInterfaces[_interfaceId]; \n\
  \     } \n\
  \ } \n\
  \"

iDiamondCut :: T.Text
iDiamondCut =
  " \
  \ // SPDX-License-Identifier: MIT \n\
  \ pragma solidity ^0.8.0; \n\
  \  \n\
  \ /******************************************************************************\ \n\
  \ * Author: Nick Mudge <nick@perfectabstractions.com> (https://twitter.com/mudgen) \n\
  \ * EIP-2535 Diamonds: https://eips.ethereum.org/EIPS/eip-2535 \n\
  \ /******************************************************************************/ \n\
  \ \n\
  \ interface IDiamondCut { \n\
  \      enum FacetCutAction {Add, Replace, Remove} \n\
  \      // Add=0, Replace=1, Remove=2 \n\
  \  \n\ 
  \      struct FacetCut { \n\
  \          address facetAddress; \n\
  \          FacetCutAction action; \n\
  \          bytes4[] functionSelectors; \n\
  \      } \n\
  \ \n\
  \      /// @notice Add/replace/remove any number of functions and optionally execute \n\ 
  \      ///         a function with delegatecall \n\
  \      /// @param _diamondCut Contains the facet addresses and function selectors \n\
  \      /// @param _init The address of the contract or facet to execute _calldata \n\
  \      /// @param _calldata A function call, including function selector and arguments \n\
  \      ///                  _calldata is executed with delegatecall on _init \n\
  \      function diamondCut( \n\
  \          FacetCut[] calldata _diamondCut, \n\
  \          address _init, \n\
  \          bytes calldata _calldata \n\
  \      ) external; \n\
  \ \n\
  \      event DiamondCut(FacetCut[] _diamondCut, address _init, bytes _calldata); \n\
  \  } \n\
  \"

iDiamondLoupe :: T.Text
iDiamondLoupe =
  " \
  \  // SPDX-License-Identifier: MIT \n\
  \  pragma solidity ^0.8.0; \n\
  \  \n\ 
  \  /******************************************************************************\ \n\
  \  * Author: Nick Mudge <nick@perfectabstractions.com> (https://twitter.com/mudgen) \n\
  \  * EIP-2535 Diamonds: https://eips.ethereum.org/EIPS/eip-2535 \n\
  \  /******************************************************************************/ \n\
  \  \n\
  \  // A loupe is a small magnifying glass used to look at diamonds. \n\
  \  // These functions look at diamonds \n\
  \  interface IDiamondLoupe { \n\
  \      /// These functions are expected to be called frequently \n\
  \      /// by tools. \n\
  \ \n\
  \      struct Facet { \n\
  \          address facetAddress; \n\
  \          bytes4[] functionSelectors; \n\
  \      } \n\
  \ \n\
  \      /// @notice Gets all facet addresses and their four byte function selectors. \n\
  \      /// @return facets_ Facet \n\
  \      function facets() external view returns (Facet[] memory facets_); \n\
  \ \n\
  \      /// @notice Gets all the function selectors supported by a specific facet. \n\
  \      /// @param _facet The facet address. \n\
  \      /// @return facetFunctionSelectors_ \n\
  \      function facetFunctionSelectors(address _facet) external view returns (bytes4[] memory facetFunctionSelectors_); \n\
  \  \n\
  \      /// @notice Get all the facet addresses used by a diamond. \n\
  \      /// @return facetAddresses_ \n\
  \      function facetAddresses() external view returns (address[] memory facetAddresses_); \n\ 
  \ \n\
  \      /// @notice Gets the facet that supports the given selector. \n\
  \      /// @dev If facet is not found return address(0). \n\
  \      /// @param _functionSelector The function selector. \n\
  \      /// @return facetAddress_ The facet address. \n\
  \      function facetAddress(bytes4 _functionSelector) external view returns (address facetAddress_); \n\
  \  } \n\
  \"

ierc165 :: T.Text
ierc165 =
  "\
  \  // SPDX-License-Identifier: MIT \n\
  \  pragma solidity ^0.8.0; \n\
  \   \n\
  \  interface IERC165 { \n\
  \      /// @notice Query if a contract implements an interface \n\
  \      /// @param interfaceId The interface identifier, as specified in ERC-165 \n\ 
  \      /// @dev Interface identification is specified in ERC-165. This function \n\
  \      ///  uses less than 30,000 gas. \n\
  \      /// @return `true` if the contract implements `interfaceID` and \n\
  \      ///  `interfaceID` is not 0xffffffff, `false` otherwise \n\
  \      function supportsInterface(bytes4 interfaceId) external view returns (bool); \n\
  \  } \n\
  \"

libDiamond :: T.Text
libDiamond =
  "\
  \ // SPDX-License-Identifier: MIT \n\
  \ pragma solidity ^0.8.0; \n\
  \  \n\
  \ /******************************************************************************\ \n\
  \ * Author: Nick Mudge <nick@perfectabstractions.com> (https://twitter.com/mudgen) \n\
  \ * EIP-2535 Diamonds: https://eips.ethereum.org/EIPS/eip-2535 \n\
  \ /******************************************************************************/ \n\
  \ import { IDiamondCut } from \"./IDiamondCut.sol\"; \n\
  \ \n\
  \ // Remember to add the loupe functions from DiamondLoupeFacet to the diamond. \n\
  \ // The loupe functions are required by the EIP2535 Diamonds standard \n\
  \  \n\
  \ error InitializationFunctionReverted(address _initializationContractAddress, bytes _calldata); \n\
  \  \n\
  \ library LibDiamond { \n\
  \     bytes32 constant DIAMOND_STORAGE_POSITION = keccak256(\"diamond.standard.diamond.storage\"); \n\
  \  \n\
  \     struct FacetAddressAndPosition { \n\
  \         address facetAddress; \n\
  \         uint96 functionSelectorPosition; // position in facetFunctionSelectors.functionSelectors array \n\
  \     } \n\
  \  \n\
  \     struct FacetFunctionSelectors { \n\
  \         bytes4[] functionSelectors; \n\
  \         uint256 facetAddressPosition; // position of facetAddress in facetAddresses array \n\
  \     } \n\
  \  \n\
  \     struct DiamondStorage { \n\
  \         // maps function selector to the facet address and \n\
  \         // the position of the selector in the facetFunctionSelectors.selectors array \n\
  \         mapping(bytes4 => FacetAddressAndPosition) selectorToFacetAndPosition; \n\ 
  \         // maps facet addresses to function selectors \n\
  \         mapping(address => FacetFunctionSelectors) facetFunctionSelectors; \n\
  \         // facet addresses \n\
  \         address[] facetAddresses; \n\ 
  \         // Used to query if a contract implements an interface. \n\
  \         // Used to implement ERC-165. \n\
  \         mapping(bytes4 => bool) supportedInterfaces; \n\ 
  \         // owner of the contract \n\
  \         address contractOwner; \n\
  \     } \n\
  \  \n\
  \     function diamondStorage() internal pure returns (DiamondStorage storage ds) { \n\
  \         bytes32 position = DIAMOND_STORAGE_POSITION; \n\
  \         assembly { \n\
  \             ds.slot := position \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     event OwnershipTransferred(address indexed previousOwner, address indexed newOwner); \n\
  \  \n\
  \     function setContractOwner(address _newOwner) internal { \n\
  \         DiamondStorage storage ds = diamondStorage(); \n\
  \         address previousOwner = ds.contractOwner; \n\ 
  \         ds.contractOwner = _newOwner; \n\
  \         emit OwnershipTransferred(previousOwner, _newOwner); \n\ 
  \     } \n\
  \  \n\
  \     function contractOwner() internal view returns (address contractOwner_) { \n\
  \         contractOwner_ = diamondStorage().contractOwner; \n\
  \     } \n\
  \  \n\
  \     function enforceIsContractOwner() internal view { \n\
  \         require(msg.sender == diamondStorage().contractOwner, \"LibDiamond: Must be contract owner\"); \n\
  \     } \n\
  \  \n\
  \     event DiamondCut(IDiamondCut.FacetCut[] _diamondCut, address _init, bytes _calldata); \n\
  \  \n\
  \     // Internal function version of diamondCut \n\ 
  \     function diamondCut( \n\
  \         IDiamondCut.FacetCut[] memory _diamondCut, \n\
  \         address _init, \n\
  \         bytes memory _calldata \n\
  \     ) internal { \n\
  \         for (uint256 facetIndex; facetIndex < _diamondCut.length; facetIndex++) { \n\ 
  \             IDiamondCut.FacetCutAction action = _diamondCut[facetIndex].action; \n\
  \             if (action == IDiamondCut.FacetCutAction.Add) { \n\
  \                 addFunctions(_diamondCut[facetIndex].facetAddress, _diamondCut[facetIndex].functionSelectors); \n\
  \             } else if (action == IDiamondCut.FacetCutAction.Replace) { \n\
  \                 replaceFunctions(_diamondCut[facetIndex].facetAddress, _diamondCut[facetIndex].functionSelectors); \n\
  \             } else if (action == IDiamondCut.FacetCutAction.Remove) { \n\
  \                 removeFunctions(_diamondCut[facetIndex].facetAddress, _diamondCut[facetIndex].functionSelectors); \n\
  \             } else { \n\
  \                 revert(\"LibDiamondCut: Incorrect FacetCutAction\"); \n\
  \             } \n\
  \         } \n\
  \         emit DiamondCut(_diamondCut, _init, _calldata); \n\
  \         initializeDiamondCut(_init, _calldata); \n\
  \     } \n\
  \ \n\
  \     function addFunctions(address _facetAddress, bytes4[] memory _functionSelectors) internal { \n\
  \         require(_functionSelectors.length > 0, \"LibDiamondCut: No selectors in facet to cut\"); \n\
  \         DiamondStorage storage ds = diamondStorage();         \n\
  \         require(_facetAddress != address(0), \"LibDiamondCut: Add facet can't be address(0)\"); \n\
  \         uint96 selectorPosition = uint96(ds.facetFunctionSelectors[_facetAddress].functionSelectors.length); \n\
  \         // add new facet address if it does not exist \n\ 
  \         if (selectorPosition == 0) { \n\
  \             addFacet(ds, _facetAddress);             \n\
  \         } \n\
  \         for (uint256 selectorIndex; selectorIndex < _functionSelectors.length; selectorIndex++) { \n\
  \             bytes4 selector = _functionSelectors[selectorIndex]; \n\
  \             address oldFacetAddress = ds.selectorToFacetAndPosition[selector].facetAddress; \n\
  \             require(oldFacetAddress == address(0), \"LibDiamondCut: Can't add function that already exists\"); \n\
  \             addFunction(ds, selector, selectorPosition, _facetAddress); \n\
  \             selectorPosition++; \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     function replaceFunctions(address _facetAddress, bytes4[] memory _functionSelectors) internal { \n\
  \         require(_functionSelectors.length > 0, \"LibDiamondCut: No selectors in facet to cut\"); \n\
  \         DiamondStorage storage ds = diamondStorage(); \n\
  \         require(_facetAddress != address(0), \"LibDiamondCut: Add facet can't be address(0)\"); \n\
  \         uint96 selectorPosition = uint96(ds.facetFunctionSelectors[_facetAddress].functionSelectors.length); \n\
  \         // add new facet address if it does not exist \n\
  \         if (selectorPosition == 0) { \n\
  \             addFacet(ds, _facetAddress); \n\
  \         } \n\
  \         for (uint256 selectorIndex; selectorIndex < _functionSelectors.length; selectorIndex++) { \n\
  \             bytes4 selector = _functionSelectors[selectorIndex]; \n\
  \             address oldFacetAddress = ds.selectorToFacetAndPosition[selector].facetAddress; \n\
  \             require(oldFacetAddress != _facetAddress, \"LibDiamondCut: Can't replace function with same function\"); \n\
  \             removeFunction(ds, oldFacetAddress, selector); \n\
  \             addFunction(ds, selector, selectorPosition, _facetAddress); \n\
  \             selectorPosition++; \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     function removeFunctions(address _facetAddress, bytes4[] memory _functionSelectors) internal { \n\
  \         require(_functionSelectors.length > 0, \"LibDiamondCut: No selectors in facet to cut\"); \n\
  \         DiamondStorage storage ds = diamondStorage(); \n\ 
  \         // if function does not exist then do nothing and return \n\
  \         require(_facetAddress == address(0), \"LibDiamondCut: Remove facet address must be address(0)\"); \n\
  \         for (uint256 selectorIndex; selectorIndex < _functionSelectors.length; selectorIndex++) { \n\
  \             bytes4 selector = _functionSelectors[selectorIndex]; \n\
  \             address oldFacetAddress = ds.selectorToFacetAndPosition[selector].facetAddress; \n\
  \             removeFunction(ds, oldFacetAddress, selector); \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     function addFacet(DiamondStorage storage ds, address _facetAddress) internal { \n\
  \         enforceHasContractCode(_facetAddress, \"LibDiamondCut: New facet has no code\"); \n\
  \         ds.facetFunctionSelectors[_facetAddress].facetAddressPosition = ds.facetAddresses.length; \n\
  \         ds.facetAddresses.push(_facetAddress); \n\
  \     }     \n\
  \  \n\
  \     function addFunction(DiamondStorage storage ds, bytes4 _selector, uint96 _selectorPosition, address _facetAddress) internal { \n\
  \         ds.selectorToFacetAndPosition[_selector].functionSelectorPosition = _selectorPosition; \n\
  \         ds.facetFunctionSelectors[_facetAddress].functionSelectors.push(_selector); \n\
  \         ds.selectorToFacetAndPosition[_selector].facetAddress = _facetAddress; \n\
  \     } \n\
  \  \n\
  \     function removeFunction(DiamondStorage storage ds, address _facetAddress, bytes4 _selector) internal {       \n\  
  \         require(_facetAddress != address(0), \"LibDiamondCut: Can't remove function that doesn't exist\"); \n\
  \         // an immutable function is a function defined directly in a diamond \n\
  \         require(_facetAddress != address(this), \"LibDiamondCut: Can't remove immutable function\"); \n\ 
  \         // replace selector with last selector, then delete last selector \n\
  \         uint256 selectorPosition = ds.selectorToFacetAndPosition[_selector].functionSelectorPosition; \n\
  \         uint256 lastSelectorPosition = ds.facetFunctionSelectors[_facetAddress].functionSelectors.length - 1; \n\
  \         // if not the same then replace _selector with lastSelector \n\
  \         if (selectorPosition != lastSelectorPosition) { \n\
  \             bytes4 lastSelector = ds.facetFunctionSelectors[_facetAddress].functionSelectors[lastSelectorPosition]; \n\ 
  \             ds.facetFunctionSelectors[_facetAddress].functionSelectors[selectorPosition] = lastSelector; \n\
  \             ds.selectorToFacetAndPosition[lastSelector].functionSelectorPosition = uint96(selectorPosition); \n\
  \         } \n\
  \         // delete the last selector \n\
  \         ds.facetFunctionSelectors[_facetAddress].functionSelectors.pop(); \n\
  \         delete ds.selectorToFacetAndPosition[_selector]; \n\
  \  \n\
  \         // if no more selectors for facet address then delete the facet address \n\
  \         if (lastSelectorPosition == 0) { \n\
  \             // replace facet address with last facet address and delete last facet address \n\
  \             uint256 lastFacetAddressPosition = ds.facetAddresses.length - 1; \n\
  \             uint256 facetAddressPosition = ds.facetFunctionSelectors[_facetAddress].facetAddressPosition; \n\
  \             if (facetAddressPosition != lastFacetAddressPosition) { \n\ 
  \                 address lastFacetAddress = ds.facetAddresses[lastFacetAddressPosition]; \n\
  \                 ds.facetAddresses[facetAddressPosition] = lastFacetAddress; \n\
  \                 ds.facetFunctionSelectors[lastFacetAddress].facetAddressPosition = facetAddressPosition; \n\
  \             } \n\
  \             ds.facetAddresses.pop(); \n\
  \             delete ds.facetFunctionSelectors[_facetAddress].facetAddressPosition; \n\
  \         } \n\
  \     } \n\
  \  \n\
  \     function initializeDiamondCut(address _init, bytes memory _calldata) internal { \n\
  \         if (_init == address(0)) { \n\
  \             return; \n\
  \         } \n\
  \         enforceHasContractCode(_init, \"LibDiamondCut: _init address has no code\");         \n\
  \         (bool success, bytes memory error) = _init.delegatecall(_calldata); \n\
  \         if (!success) { \n\
  \             if (error.length > 0) { \n\
  \                 // bubble up error \n\
  \                 /// @solidity memory-safe-assembly \n\
  \                 assembly { \n\
  \                     let returndata_size := mload(error) \n\
  \                     revert(add(32, error), returndata_size) \n\
  \                 } \n\
  \             } else { \n\
  \                 revert InitializationFunctionReverted(_init, _calldata); \n\
  \             }\n\
  \         } \n\ 
  \     } \n\
  \  \n\
  \     function enforceHasContractCode(address _contract, string memory _errorMessage) internal view { \n\
  \         uint256 contractSize; \n\
  \         assembly { \n\
  \             contractSize := extcodesize(_contract) \n\
  \         } \n\
  \         require(contractSize > 0, _errorMessage); \n\
  \     } \n\
  \ } \n\
  \  \n\
  \"
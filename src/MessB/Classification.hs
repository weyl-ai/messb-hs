{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MessB.Classification (
    classifyStackFrame,
    classifyByFilePath,
    classifyByFunctionName,
    isUserCodePath,
    isLibraryInternalPath,
    isFlakeSourcePath,
    isModuleSystemFunction,
    buildDefaultOntology,
) where

import Data.Foldable (asum)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import MessB.Types

{- | Classify a stack frame using three-tier fallback strategy

Classification priority (highest to lowest):
  1. Explicit role annotation - return immediately with high confidence
  2. Symbol table lookup - O(1) check in ontology by function name
  3. Path pattern matching - heuristic based on file path

Invariants:
  - UserCode frames are NEVER misclassified as LibraryInternal
  - Classification is deterministic for (frame, ontology) pair
  - Unknown frames return LowConfidence, never guess randomly
-}
classifyStackFrame ::
    ClassificationOntology ->
    StackFrame ->
    (FrameClassification, ConfidenceLevel)
classifyStackFrame (ClassificationOntology ontology) StackFrame{..} =
    fromMaybe (Unknown, LowConfidence) $
        asum
            [ (,HighConfidence) <$> frameExplicitRole
            , (,HighConfidence) <$> (frameFunctionName >>= (`Map.lookup` ontology))
            , classifyByFilePath <$> frameFilePath
            ]

-- | Classify based on file path patterns
classifyByFilePath :: Text -> (FrameClassification, ConfidenceLevel)
classifyByFilePath filePath
    | isFlakeSourcePath filePath && not (isKnownLibrarySubpath filePath) =
        (UserCode, MediumConfidence)
    | isNixStorePath filePath =
        (LibraryInternal, MediumConfidence)
    | isUserCodePath filePath =
        (UserCode, HighConfidence)
    | isModuleSystemPath filePath =
        (ModuleSystem, MediumConfidence)
    | otherwise =
        (Unknown, LowConfidence)

-- | Check if path is a flake source (user code copied to store)
isFlakeSourcePath :: Text -> Bool
isFlakeSourcePath filePath = "-source/" `Text.isInfixOf` filePath

-- | Check if path is within /nix/store/
isNixStorePath :: Text -> Bool
isNixStorePath filePath = "/nix/store/" `Text.isPrefixOf` filePath

-- | Check if path is a known user code location
isUserCodePath :: Text -> Bool
isUserCodePath filePath =
    any
        (`Text.isPrefixOf` filePath)
        [ "/home/"
        , "/etc/nixos/"
        , "/etc/darwin/"
        , "/Users/"
        ]

-- | Check if path is a library internal path
isLibraryInternalPath :: Text -> Bool
isLibraryInternalPath filePath =
    isNixStorePath filePath && not (isFlakeSourcePath filePath)

-- | Check if path is known library subpath (not user code even in -source/)
isKnownLibrarySubpath :: Text -> Bool
isKnownLibrarySubpath filePath =
    any
        (`Text.isInfixOf` filePath)
        [ "lib/"
        , "nixos/"
        , "pkgs/"
        , "nixpkgs/"
        ]

-- | Check if path is module system internals
isModuleSystemPath :: Text -> Bool
isModuleSystemPath filePath =
    any
        (`Text.isInfixOf` filePath)
        [ "lib/modules.nix"
        , "lib/options.nix"
        , "lib/types.nix"
        ]

-- | Check if function name is a module system function
isModuleSystemFunction :: Text -> Bool
isModuleSystemFunction functionName =
    functionName
        `elem` [ "mergeModules"
               , "evalModules"
               , "mergeDefinitions"
               , "mergeOptionDecls"
               , "filterOverrides"
               , "byName"
               , "pushDownProperties"
               , "dischargeProperties"
               , "fixupOptionType"
               ]

-- | Classify based on function name alone
classifyByFunctionName ::
    ClassificationOntology ->
    Text ->
    Maybe (FrameClassification, ConfidenceLevel)
classifyByFunctionName (ClassificationOntology ontology) functionName =
    (,HighConfidence) <$> Map.lookup functionName ontology

-- | Build the default classification ontology
buildDefaultOntology :: ClassificationOntology
buildDefaultOntology =
    ClassificationOntology $
        Map.fromList
            -- Module system functions
            [ ("mergeModules", ModuleSystem)
            , ("evalModules", ModuleSystem)
            , ("mergeDefinitions", ModuleSystem)
            , ("mergeOptionDecls", ModuleSystem)
            , ("filterOverrides", ModuleSystem)
            , ("byName", ModuleSystem)
            , ("pushDownProperties", ModuleSystem)
            , ("dischargeProperties", ModuleSystem)
            , ("fixupOptionType", ModuleSystem)
            , ("mkOptionType", ModuleSystem)
            , ("mkOption", ModuleSystem)
            , ("mkEnableOption", ModuleSystem)
            , ("mkPackageOption", ModuleSystem)
            , ("mkIf", ModuleSystem)
            , ("mkMerge", ModuleSystem)
            , ("mkOverride", ModuleSystem)
            , ("mkDefault", ModuleSystem)
            , ("mkForce", ModuleSystem)
            , -- Fixed point functions
              ("fix", FixedPoint)
            , ("fix'", FixedPoint)
            , ("extends", FixedPoint)
            , ("composeManyExtensions", FixedPoint)
            , ("makeExtensible", FixedPoint)
            , ("makeExtensibleWithCustomName", FixedPoint)
            , -- Overlay functions
              ("applyOverlays", Overlay)
            , ("composeExtensions", Overlay)
            , ("overlayPackages", Overlay)
            , -- Package set functions
              ("callPackage", PackageSet)
            , ("callPackageWith", PackageSet)
            , ("callPackagesWith", PackageSet)
            , ("mkDerivation", PackageSet)
            , ("stdenv.mkDerivation", PackageSet)
            , ("buildPythonPackage", PackageSet)
            , ("buildGoModule", PackageSet)
            , ("buildRustPackage", PackageSet)
            , -- Library functions (internal)
              ("mapAttrs", LibraryInternal)
            , ("mapAttrs'", LibraryInternal)
            , ("filterAttrs", LibraryInternal)
            , ("genAttrs", LibraryInternal)
            , ("listToAttrs", LibraryInternal)
            , ("attrValues", LibraryInternal)
            , ("attrNames", LibraryInternal)
            , ("concatMap", LibraryInternal)
            , ("concatMapStringsSep", LibraryInternal)
            , ("foldl'", LibraryInternal)
            , ("foldr", LibraryInternal)
            ]

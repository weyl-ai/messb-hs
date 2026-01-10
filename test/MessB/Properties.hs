{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MessB.Properties (
    propertyTests,
) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import MessB.Classification
import MessB.Filter (filterTrace)
import MessB.Types

-- | All property tests
propertyTests :: Group
propertyTests =
    Group
        "MessB.Properties"
        [ ("prop_classification_deterministic", propClassificationDeterministic)
        , ("prop_never_misclassify_user_as_internal", propNeverMisclassifyUserAsInternal)
        , ("prop_explicit_role_always_wins", propExplicitRoleAlwaysWins)
        , ("prop_ontology_lookup_high_confidence", propOntologyLookupHighConfidence)
        , ("prop_filtering_reduces_frame_count", propFilteringReducesFrameCount)
        , ("prop_boundaries_always_retained", propBoundariesAlwaysRetained)
        ]

-- | Classification is deterministic for same inputs
propClassificationDeterministic :: Property
propClassificationDeterministic = property $ do
    frame <- forAll genStackFrame
    ontology <- forAll genOntology
    let result1 = classifyStackFrame ontology frame
    let result2 = classifyStackFrame ontology frame
    result1 === result2

-- | User code paths are never classified as library internal
propNeverMisclassifyUserAsInternal :: Property
propNeverMisclassifyUserAsInternal = property $ do
    userPath <- forAll genUserCodePath
    let frame = emptyStackFrame{frameFilePath = Just userPath}
    ontology <- forAll genOntology
    let (classification, _) = classifyStackFrame ontology frame
    assert $ classification /= LibraryInternal

-- | Explicit role annotation always takes precedence
propExplicitRoleAlwaysWins :: Property
propExplicitRoleAlwaysWins = property $ do
    explicitRole <- forAll genFrameClassification
    frame <- forAll genStackFrame
    let frameWithRole = frame{frameExplicitRole = Just explicitRole}
    ontology <- forAll genOntology
    let (classification, confidence) = classifyStackFrame ontology frameWithRole
    classification === explicitRole
    confidence === HighConfidence

-- | Ontology lookup returns high confidence
propOntologyLookupHighConfidence :: Property
propOntologyLookupHighConfidence = property $ do
    functionName <- forAll genFunctionName
    classification <- forAll genFrameClassification
    let ontology = buildOntologyWith functionName classification
    let frame = emptyStackFrame{frameFunctionName = Just functionName}
    let (resultClassification, confidence) = classifyStackFrame ontology frame
    resultClassification === classification
    confidence === HighConfidence

-- | Filtering never increases frame count
propFilteringReducesFrameCount :: Property
propFilteringReducesFrameCount = property $ do
    frameCount <- forAll $ Gen.int (Range.linear 1 100)
    frames <- forAll $ Gen.list (Range.singleton frameCount) genStackFrame
    let trace =
            StackTrace
                { traceErrorMessage = "test error"
                , traceErrorCategory = UnknownError
                , traceFrames = frames
                , traceRawOutput = ""
                }
    let config = emptyFilterConfiguration{configMaxFrames = 10}
    let filtered = filterTrace config trace
    let retainedCount = countRetainedFrames (filteredFrames filtered)
    assert $ retainedCount <= frameCount
  where
    countRetainedFrames = length . filter isRetained
    isRetained FrameRetained{} = True
    isRetained (FrameBoundary _ _) = True
    isRetained (FrameElided _) = False

-- | Boundary frames are always retained
propBoundariesAlwaysRetained :: Property
propBoundariesAlwaysRetained = property $ do
    frames <- forAll $ Gen.list (Range.linear 2 20) genStackFrame
    let trace =
            StackTrace
                { traceErrorMessage = "test error"
                , traceErrorCategory = UnknownError
                , traceFrames = frames
                , traceRawOutput = ""
                }
    let config =
            emptyFilterConfiguration
                { configPreserveBoundaries = True
                , configMaxFrames = 5
                }
    let filtered = filterTrace config trace
    let boundaryCount = length (filteredBoundaries filtered)
    let retainedBoundaries = length $ filter isBoundaryResult (filteredFrames filtered)
    -- All detected boundaries should be retained
    boundaryCount === retainedBoundaries
  where
    isBoundaryResult (FrameBoundary _ _) = True
    isBoundaryResult _ = False

-- Generators

genStackFrame :: Gen StackFrame
genStackFrame = do
    filePath <- Gen.maybe genFilePath
    functionName <- Gen.maybe genFunctionName
    message <- Gen.maybe genMessage
    explicitRole <- Gen.maybe genFrameClassification
    pure
        StackFrame
            { frameFilePath = filePath
            , frameFunctionName = functionName
            , frameMessage = message
            , frameSourceLocation = Nothing
            , frameExplicitRole = explicitRole
            , frameRawText = ""
            }

genFilePath :: Gen Text
genFilePath =
    Gen.choice
        [ genUserCodePath
        , genLibraryPath
        , genFlakeSourcePath
        , genArbitraryPath
        ]

genUserCodePath :: Gen Text
genUserCodePath = do
    prefix <- Gen.element ["/home/user/", "/etc/nixos/", "/etc/darwin/", "/Users/dev/"]
    suffix <- genPathSuffix
    pure $ prefix <> suffix

genLibraryPath :: Gen Text
genLibraryPath = do
    hash <- Gen.text (Range.singleton 32) Gen.alphaNum
    suffix <- genPathSuffix
    pure $ "/nix/store/" <> hash <> "-nixpkgs/" <> suffix

genFlakeSourcePath :: Gen Text
genFlakeSourcePath = do
    hash <- Gen.text (Range.singleton 32) Gen.alphaNum
    suffix <- genPathSuffix
    pure $ "/nix/store/" <> hash <> "-source/" <> suffix

genArbitraryPath :: Gen Text
genArbitraryPath = do
    parts <-
        Gen.list (Range.linear 1 5) $
            Gen.text (Range.linear 1 10) Gen.alpha
    pure $ "/" <> Text.intercalate "/" parts <> ".nix"

genPathSuffix :: Gen Text
genPathSuffix = do
    parts <-
        Gen.list (Range.linear 1 3) $
            Gen.text (Range.linear 1 10) Gen.alpha
    pure $ Text.intercalate "/" parts <> ".nix"

genFunctionName :: Gen Text
genFunctionName =
    Gen.choice
        [ Gen.element knownFunctions
        , Gen.text (Range.linear 1 20) Gen.alpha
        ]

knownFunctions :: [Text]
knownFunctions =
    [ "mergeModules"
    , "evalModules"
    , "callPackage"
    , "mkDerivation"
    , "mapAttrs"
    , "fix"
    , "extends"
    , "applyOverlays"
    ]

genMessage :: Gen Text
genMessage = Gen.text (Range.linear 10 100) Gen.unicode

genFrameClassification :: Gen FrameClassification
genFrameClassification = Gen.enumBounded

-- TODO: Use in additional property tests
_genConfidenceLevel :: Gen ConfidenceLevel
_genConfidenceLevel = Gen.enumBounded

genOntology :: Gen ClassificationOntology
genOntology = do
    entries <- Gen.list (Range.linear 0 20) $ do
        name <- genFunctionName
        classification <- genFrameClassification
        pure (name, classification)
    pure $ ClassificationOntology $ Map.fromList entries

buildOntologyWith :: Text -> FrameClassification -> ClassificationOntology
buildOntologyWith name classification =
    ClassificationOntology $ Map.singleton name classification

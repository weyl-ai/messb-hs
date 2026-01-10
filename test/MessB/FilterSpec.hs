{-# LANGUAGE OverloadedStrings #-}

module MessB.FilterSpec (spec) where

import Test.Hspec

import MessB.Classification (buildDefaultOntology)
import MessB.Filter
import MessB.Types

spec :: Spec
spec = describe "MessB.Filter" $ do
    describe "filterTrace" $ do
        it "reduces frame count according to maxFrames" $ do
            let frames = replicate 50 emptyStackFrame
            let trace = makeTrace frames
            let config = emptyFilterConfiguration{configMaxFrames = 10}
            let filtered = filterTrace config trace
            let retainedCount = countRetained (filteredFrames filtered)
            retainedCount `shouldSatisfy` (<= 10)

        it "preserves boundaries when configured" $ do
            let userFrame =
                    emptyStackFrame
                        { frameFilePath = Just "/home/user/config.nix"
                        }
            let libraryFrame =
                    emptyStackFrame
                        { frameFilePath = Just "/nix/store/abc/lib.nix"
                        }
            let frames = [libraryFrame, libraryFrame, userFrame, libraryFrame]
            let trace = makeTrace frames
            let config =
                    emptyFilterConfiguration
                        { configPreserveBoundaries = True
                        , configOntology = buildDefaultOntology
                        }
            let filtered = filterTrace config trace
            length (filteredBoundaries filtered) `shouldSatisfy` (> 0)

        it "generates hints for known patterns" $ do
            let frame =
                    emptyStackFrame
                        { frameMessage = Just "while evaluating the module argument 'stdenv'"
                        , frameFunctionName = Just "evalModules"
                        }
            let trace =
                    StackTrace
                        { traceErrorMessage = "infinite recursion"
                        , traceErrorCategory = InfiniteRecursion
                        , traceFrames = [frame]
                        , traceRawOutput = ""
                        }
            let config = emptyFilterConfiguration{configIncludeHints = True}
            let filtered = filterTrace config trace
            length (filteredHints filtered) `shouldSatisfy` (> 0)

        it "calculates correct statistics" $ do
            let frames = replicate 20 emptyStackFrame
            let trace = makeTrace frames
            let config = emptyFilterConfiguration{configMaxFrames = 5}
            let filtered = filterTrace config trace
            let stats = filteredStatistics filtered
            statsOriginalFrameCount stats `shouldBe` 20

    describe "selectRelevantFrames" $ do
        it "prefers user code frames" $ do
            let userFrame = (0, emptyStackFrame{frameFilePath = Just "/home/user/f.nix"}, UserCode, HighConfidence)
            let libFrame = (1, emptyStackFrame{frameFilePath = Just "/nix/store/x/f.nix"}, LibraryInternal, MediumConfidence)
            let selected = selectRelevantFrames 1 [libFrame, userFrame] []
            length selected `shouldBe` 1
            case selected of
                [(_, frame, classification, _)] -> do
                    frameFilePath frame `shouldBe` Just "/home/user/f.nix"
                    classification `shouldBe` UserCode
                _ -> expectationFailure "Expected one frame"

        it "always includes boundary indices" $ do
            let frames =
                    [ (0, emptyStackFrame, LibraryInternal, LowConfidence)
                    , (1, emptyStackFrame, LibraryInternal, LowConfidence)
                    , (2, emptyStackFrame, LibraryInternal, LowConfidence)
                    ]
            let boundaryIndices = [1] -- Frame 1 is a boundary
            let selected = selectRelevantFrames 2 frames boundaryIndices
            let selectedIndices = map (\(frameIndex, _, _, _) -> frameIndex) selected
            1 `shouldSatisfy` (`elem` selectedIndices)

    describe "calculateStatistics" $ do
        it "calculates reduction percentage" $ do
            let original = replicate 100 emptyStackFrame
            let results =
                    [ FrameRetained emptyStackFrame UserCode HighConfidence
                    , FrameRetained emptyStackFrame UserCode HighConfidence
                    ]
            let stats = calculateStatistics original results
            statsOriginalFrameCount stats `shouldBe` 100
            statsRetainedFrameCount stats `shouldBe` 2
            statsReductionPercent stats `shouldSatisfy` (> 90)

    describe "applyReductionStrategy" $ do
        it "KeepBoundariesAndUserCode keeps only boundaries and user code" $ do
            let frames =
                    [ (0, emptyStackFrame, UserCode, HighConfidence)
                    , (1, emptyStackFrame, LibraryInternal, MediumConfidence)
                    , (2, emptyStackFrame, ModuleSystem, HighConfidence)
                    ]
            let result = applyReductionStrategy KeepBoundariesAndUserCode frames [2]
            length result `shouldBe` 2 -- UserCode and boundary
        it "KeepHighConfidenceOnly filters by confidence" $ do
            let frames =
                    [ (0, emptyStackFrame, UserCode, HighConfidence)
                    , (1, emptyStackFrame, UserCode, LowConfidence)
                    , (2, emptyStackFrame, UserCode, MediumConfidence)
                    ]
            let result = applyReductionStrategy KeepHighConfidenceOnly frames []
            length result `shouldBe` 1

-- Helpers

makeTrace :: [StackFrame] -> StackTrace
makeTrace frames =
    StackTrace
        { traceErrorMessage = "test error"
        , traceErrorCategory = UnknownError
        , traceFrames = frames
        , traceRawOutput = ""
        }

countRetained :: [FilterResult] -> Int
countRetained = length . filter isRetained
  where
    isRetained FrameRetained{} = True
    isRetained (FrameBoundary _ _) = True
    isRetained _ = False

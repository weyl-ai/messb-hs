{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MessB.Filter (
    filterTrace,
    filterFrames,
    selectRelevantFrames,
    calculateStatistics,
    applyReductionStrategy,
    ReductionStrategy (..),
) where

import Data.List (sortOn)

import MessB.Boundary
import MessB.Classification
import MessB.Hints
import MessB.Types

-- | Strategy for reducing frame count
data ReductionStrategy
    = KeepBoundariesAndUserCode
    | KeepFirstAndLast
    | KeepHighConfidenceOnly
    | KeepAll
    deriving stock (Eq, Show)

-- | Filter a complete stack trace
filterTrace :: FilterConfiguration -> StackTrace -> FilteredTrace
filterTrace configuration@FilterConfiguration{..} trace@StackTrace{..} =
    let ontology = configOntology
        boundaries = detectBoundaries ontology traceFrames
        hints =
            if configIncludeHints
                then generateHints ontology trace
                else []
        filteredFrameResults = filterFrames configuration traceFrames boundaries
        statistics = calculateStatistics traceFrames filteredFrameResults
     in FilteredTrace
            { filteredErrorMessage = traceErrorMessage
            , filteredErrorCategory = traceErrorCategory
            , filteredFrames = filteredFrameResults
            , filteredBoundaries = boundaries
            , filteredHints = hints
            , filteredStatistics = statistics
            }

-- | Filter frames based on configuration
filterFrames ::
    FilterConfiguration ->
    [StackFrame] ->
    [Boundary] ->
    [FilterResult]
filterFrames FilterConfiguration{..} frames boundaries =
    let ontology = configOntology
        classified = zipWith (classifyWithIndex ontology) [0 ..] frames
        boundaryIndices = map boundaryFrameIndex boundaries
        selected = selectRelevantFrames configMaxFrames classified boundaryIndices
     in map (toFilterResult boundaries) selected
  where
    classifyWithIndex ontology frameIndex frame =
        let (classification, confidence) = classifyStackFrame ontology frame
         in (frameIndex, frame, classification, confidence)

    toFilterResult allBoundaries (frameIndex, frame, classification, confidence) =
        case filter ((== frameIndex) . boundaryFrameIndex) allBoundaries of
            (boundary : _) -> FrameBoundary frame boundary
            [] -> FrameRetained frame classification confidence

-- | Select which frames to retain
selectRelevantFrames ::
    -- | Maximum frame count
    Int ->
    [(Int, StackFrame, FrameClassification, ConfidenceLevel)] ->
    -- | Boundary indices (must be retained)
    [Int] ->
    [(Int, StackFrame, FrameClassification, ConfidenceLevel)]
selectRelevantFrames maxFrames classified boundaryIndices =
    let withScores = map (scoreFrame boundaryIndices) classified
        sorted = sortOn (negate . fst) withScores
        selected = take maxFrames sorted
        reordered = sortOn (\(_, (frameIndex, _, _, _)) -> frameIndex) selected
     in map snd reordered

-- | Score a frame for relevance (higher = more relevant)
scoreFrame ::
    [Int] ->
    (Int, StackFrame, FrameClassification, ConfidenceLevel) ->
    (Int, (Int, StackFrame, FrameClassification, ConfidenceLevel))
scoreFrame boundaryIndices classified@(frameIndex, _, classification, confidence) =
    let baseScore = classificationScore classification
        confidenceBonus = confidenceScore confidence
        boundaryBonus = if frameIndex `elem` boundaryIndices then 100 else 0
        totalScore = baseScore + confidenceBonus + boundaryBonus
     in (totalScore, classified)

-- | Score based on classification
classificationScore :: FrameClassification -> Int
classificationScore = \case
    UserCode -> 50
    ModuleSystem -> 30
    PackageSet -> 20
    Overlay -> 20
    FixedPoint -> 10
    LibraryInternal -> 5
    Unknown -> 1

-- | Score based on confidence
confidenceScore :: ConfidenceLevel -> Int
confidenceScore = \case
    HighConfidence -> 20
    MediumConfidence -> 10
    LowConfidence -> 0

-- | Apply a reduction strategy
applyReductionStrategy ::
    ReductionStrategy ->
    [(Int, StackFrame, FrameClassification, ConfidenceLevel)] ->
    -- | Boundary indices
    [Int] ->
    [(Int, StackFrame, FrameClassification, ConfidenceLevel)]
applyReductionStrategy strategy classified boundaryIndices = case strategy of
    KeepBoundariesAndUserCode ->
        filter
            ( \(frameIndex, _, classification, _) ->
                frameIndex `elem` boundaryIndices || classification == UserCode
            )
            classified
    KeepFirstAndLast ->
        case classified of
            [] -> []
            [single] -> [single]
            (first : rest) -> [first, last rest]
    KeepHighConfidenceOnly ->
        filter (\(_, _, _, confidence) -> confidence == HighConfidence) classified
    KeepAll -> classified

-- | Calculate filtering statistics
calculateStatistics :: [StackFrame] -> [FilterResult] -> FilterStatistics
calculateStatistics originalFrames filteredResults =
    let originalCount = length originalFrames
        retainedCount = length (filter isRetained filteredResults)
        elidedCount = length (filter isElided filteredResults)
        boundaryCount = length (filter isBoundaryResult filteredResults)
        reductionPercent =
            if originalCount > 0
                then
                    fromIntegral (originalCount - retainedCount - boundaryCount)
                        / fromIntegral originalCount
                        * 100
                else 0
     in FilterStatistics
            { statsOriginalFrameCount = originalCount
            , statsRetainedFrameCount = retainedCount
            , statsElidedFrameCount = elidedCount
            , statsBoundaryCount = boundaryCount
            , statsReductionPercent = reductionPercent
            }
  where
    isRetained FrameRetained{} = True
    isRetained _ = False

    isElided (FrameElided _) = True
    isElided _ = False

    isBoundaryResult (FrameBoundary _ _) = True
    isBoundaryResult _ = False

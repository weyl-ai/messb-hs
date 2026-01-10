{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MessB.Boundary (
    detectBoundaries,
    detectSingleBoundary,
    classifyTransition,
    isBlameBoundary,
    findBlameTransferPoint,
) where

import MessB.Classification
import MessB.Types

-- | Detect all boundaries in a classified trace
detectBoundaries ::
    ClassificationOntology ->
    [StackFrame] ->
    [Boundary]
detectBoundaries ontology frames =
    let classifications = map (classifyStackFrame ontology) frames
        indexedClassifications = zip3 [0 ..] frames classifications
        pairs = zip indexedClassifications (drop 1 indexedClassifications)
     in concatMap (uncurry detectSingleBoundary) pairs

-- | Detect boundary between two adjacent frames
detectSingleBoundary ::
    (Int, StackFrame, (FrameClassification, ConfidenceLevel)) ->
    (Int, StackFrame, (FrameClassification, ConfidenceLevel)) ->
    [Boundary]
detectSingleBoundary (_, _, (classificationBefore, _)) (frameIndex, _, (classificationAfter, confidence)) =
    case classifyTransition classificationBefore classificationAfter of
        Just transition ->
            [ Boundary
                { boundaryFrameIndex = frameIndex
                , boundaryTransition = transition
                , boundaryConfidence = confidence
                }
            ]
        Nothing -> []

-- | Classify the transition between two frame classifications
classifyTransition ::
    FrameClassification ->
    FrameClassification ->
    Maybe BoundaryTransition
classifyTransition before after = case (before, after) of
    (ModuleSystem, UserCode) -> Just ModuleSystemToUserCode
    (UserCode, ModuleSystem) -> Just UserCodeToModuleSystem
    (LibraryInternal, UserCode) -> Just LibraryToUserCode
    (UserCode, LibraryInternal) -> Just UserCodeToLibrary
    (Overlay, PackageSet) -> Just OverlayToPackageSet
    (PackageSet, Overlay) -> Just PackageSetToOverlay
    -- Transitions through Unknown don't create boundaries
    (Unknown, _) -> Nothing
    (_, Unknown) -> Nothing
    -- Same classification is not a boundary
    _ | before == after -> Nothing
    -- Other transitions not significant enough
    _ -> Nothing

-- | Check if a boundary represents blame transfer
isBlameBoundary :: Boundary -> Bool
isBlameBoundary Boundary{..} = case boundaryTransition of
    ModuleSystemToUserCode -> True
    LibraryToUserCode -> True
    OverlayToPackageSet -> False
    PackageSetToOverlay -> False
    UserCodeToModuleSystem -> False
    UserCodeToLibrary -> False

-- | Find the frame index where blame transfers to user code
findBlameTransferPoint ::
    ClassificationOntology ->
    [StackFrame] ->
    Maybe Int
findBlameTransferPoint ontology frames =
    let boundaries = detectBoundaries ontology frames
        blameBoundaries = filter isBlameBoundary boundaries
     in case blameBoundaries of
            [] -> Nothing
            (firstBlameBoundary : _) -> Just (boundaryFrameIndex firstBlameBoundary)

{- | Calculate boundary confidence based on surrounding context
TODO: This function should be used in detectSingleBoundary
-}
_calculateBoundaryConfidence ::
    (FrameClassification, ConfidenceLevel) ->
    (FrameClassification, ConfidenceLevel) ->
    ConfidenceLevel
_calculateBoundaryConfidence (_, confidenceBefore) (_, confidenceAfter) =
    min confidenceBefore confidenceAfter

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MessB.Render (
    renderFilteredTrace,
    renderCompact,
    renderVerbose,
    renderFrame,
    renderBoundary,
    renderHint,
    renderStatistics,
    RenderFormat (..),
) where

import Control.Exception (IOException, catch)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import MessB.Types

-- | Output format for rendering
data RenderFormat
    = FormatCompact
    | FormatVerbose
    | FormatJson
    deriving stock (Eq, Show)

-- | Render a filtered trace to text
renderFilteredTrace :: RenderFormat -> FilteredTrace -> IO Text
renderFilteredTrace format trace = case format of
    FormatCompact -> renderCompact trace
    FormatVerbose -> renderVerbose trace
    FormatJson -> pure $ renderJson trace

-- | Compact rendering using diagnose
renderCompact :: FilteredTrace -> IO Text
renderCompact trace = do
    diagnostic <- buildDiagnosticWithFiles trace False
    pure $ renderDiagnostic diagnostic

-- | Verbose rendering using diagnose
renderVerbose :: FilteredTrace -> IO Text
renderVerbose trace = do
    diagnostic <- buildDiagnosticWithFiles trace True
    let statsText = "\n\nStatistics:\n" <> renderStatisticsText (filteredStatistics trace)
    pure $ renderDiagnostic diagnostic <> statsText

-- | Build a diagnostic and load all referenced source files
buildDiagnosticWithFiles :: FilteredTrace -> Bool -> IO (Diagnostic Text)
buildDiagnosticWithFiles trace verbose = do
    let diagnostic = buildDiagnostic trace verbose
        fileInfo = extractFileInfo trace
    loadFilesIntoDiagnostic diagnostic fileInfo

-- | Extract file paths with their maximum referenced line numbers
extractFileInfo :: FilteredTrace -> [(FilePath, Int)]
extractFileInfo FilteredTrace{..} =
    let fileLines = mapMaybe getFrameInfo filteredFrames
        grouped = foldr insertMax [] fileLines
     in grouped
  where
    getFrameInfo :: FilterResult -> Maybe (FilePath, Int)
    getFrameInfo (FrameRetained frame _ _) = getFileAndLine frame
    getFrameInfo (FrameBoundary frame _) = getFileAndLine frame
    getFrameInfo (FrameElided _) = Nothing

    getFileAndLine :: StackFrame -> Maybe (FilePath, Int)
    getFileAndLine frame = do
        path <- frameFilePath frame
        loc <- frameSourceLocation frame
        pure (Text.unpack path, locationLine loc)

    insertMax :: (FilePath, Int) -> [(FilePath, Int)] -> [(FilePath, Int)]
    insertMax (path, line) [] = [(path, line)]
    insertMax (path, line) ((p, l) : rest)
        | path == p = (p, max line l) : rest
        | otherwise = (p, l) : insertMax (path, line) rest

-- | Load files into diagnostic
loadFilesIntoDiagnostic :: Diagnostic Text -> [(FilePath, Int)] -> IO (Diagnostic Text)
loadFilesIntoDiagnostic = foldM loadFile
  where
    loadFile :: Diagnostic Text -> (FilePath, Int) -> IO (Diagnostic Text)
    loadFile d (path, maxLine) = do
        contents <- readFileOrPlaceholder path maxLine
        pure $ addFile d path (Text.unpack contents)

    readFileOrPlaceholder :: FilePath -> Int -> IO Text
    readFileOrPlaceholder path maxLine =
        TextIO.readFile path `catch` \(_ :: IOException) ->
            -- Generate placeholder content with enough lines
            -- Each line shows that the source is not available
            pure $ Text.unlines $ replicate maxLine "[source file not available]"

-- | Build a diagnose Diagnostic from our FilteredTrace
buildDiagnostic :: FilteredTrace -> Bool -> Diagnostic Text
buildDiagnostic FilteredTrace{..} verbose =
    let report = buildReport filteredErrorCategory filteredErrorMessage filteredFrames filteredHints verbose
        diagnostic = addReport mempty report
     in diagnostic

-- | Build a diagnose Report from error info
buildReport :: ErrorCategory -> Text -> [FilterResult] -> [DiagnosticHint] -> Bool -> Report Text
buildReport category message frames hints verbose =
    let isError = category /= UnknownError
        errorCode = renderErrorCode category
        markers = buildMarkers frames verbose
        hintMarkers = buildHintMarkers hints
        allMarkers = markers ++ hintMarkers
        msg =
            if Text.null message
                then "evaluation error"
                else message
        reportMsg = "[" <> errorCode <> "] " <> msg
     in if isError
            then Err (Just errorCode) reportMsg allMarkers []
            else Warn (Just errorCode) reportMsg allMarkers []

-- | Build diagnose markers from filter results
buildMarkers :: [FilterResult] -> Bool -> [(Position, Marker Text)]
buildMarkers frames verbose = concatMap (frameToMarker verbose) frames

-- | Convert a FilterResult to diagnose markers
frameToMarker :: Bool -> FilterResult -> [(Position, Marker Text)]
frameToMarker verbose result = case result of
    FrameRetained frame classification confidence ->
        let markers = stackFrameToMarkers frame
         in if verbose
                then
                    let label = Text.pack (show classification) <> " (" <> renderConfidenceText confidence <> ")"
                     in map (\(pos, _) -> (pos, Where label)) markers
                else markers -- In compact mode, don't add Where annotations to avoid empty labels
    FrameBoundary frame boundary ->
        let markers = stackFrameToMarkers frame
            label = "boundary: " <> renderTransition (boundaryTransition boundary)
         in map (\(pos, _) -> (pos, Where label)) markers
    FrameElided _ -> []

-- | Convert stack frame to position markers
stackFrameToMarkers :: StackFrame -> [(Position, Marker Text)]
stackFrameToMarkers StackFrame{..} =
    case (frameFilePath, frameSourceLocation) of
        (Just path, Just SourceLocation{..}) ->
            let pos =
                    Position
                        { begin = (locationLine, locationColumn)
                        , end = (locationLine, locationColumn + 1)
                        , file = Text.unpack path
                        }
                functionLabel = case frameFunctionName of
                    Just fn -> "in " <> fn
                    Nothing -> ""
             in [(pos, This functionLabel)]
        (Just path, Nothing) ->
            let pos =
                    Position
                        { begin = (1, 1)
                        , end = (1, 2)
                        , file = Text.unpack path
                        }
             in [(pos, This "")]
        _ -> []

-- | Build hint markers
buildHintMarkers :: [DiagnosticHint] -> [(Position, Marker Text)]
buildHintMarkers = map hintToMarker

-- | Convert diagnostic hint to Maybe marker
hintToMarker :: DiagnosticHint -> (Position, Marker Text)
hintToMarker DiagnosticHint{..} =
    -- Create a dummy position since hints aren't location-specific
    let pos = Position{begin = (0, 0), end = (0, 0), file = "<hint>"}
        label =
            hintMessage <> case hintSuggestedFix of
                Just fix -> "\n  Fix: " <> fix
                Nothing -> ""
     in (pos, Maybe label)

-- | Render a diagnose Diagnostic to Text
renderDiagnostic :: Diagnostic Text -> Text
renderDiagnostic diag =
    let doc = prettyDiagnostic WithUnicode (TabSize 4) diag
        layout = layoutPretty defaultLayoutOptions doc
     in renderStrict layout

-- | Render error category as code
renderErrorCode :: ErrorCategory -> Text
renderErrorCode = \case
    InfiniteRecursion -> "E-INFINITE-RECURSION"
    AttributeMissing -> "E-ATTRIBUTE-MISSING"
    TypeMismatch -> "E-TYPE-MISMATCH"
    AssertionFailed -> "E-ASSERTION-FAILED"
    ImportFailed -> "E-IMPORT-FAILED"
    SyntaxError -> "E-SYNTAX-ERROR"
    EvaluationError -> "E-EVALUATION"
    UnknownError -> "E-UNKNOWN"

-- | Render boundary transition
renderTransition :: BoundaryTransition -> Text
renderTransition = \case
    ModuleSystemToUserCode -> "module-system → user-code"
    UserCodeToModuleSystem -> "user-code → module-system"
    LibraryToUserCode -> "library → user-code"
    UserCodeToLibrary -> "user-code → library"
    OverlayToPackageSet -> "overlay → package-set"
    PackageSetToOverlay -> "package-set → overlay"

-- | Render confidence level
renderConfidenceText :: ConfidenceLevel -> Text
renderConfidenceText = \case
    HighConfidence -> "high"
    MediumConfidence -> "medium"
    LowConfidence -> "low"

-- | Render statistics as text
renderStatisticsText :: FilterStatistics -> Text
renderStatisticsText FilterStatistics{..} =
    "  Original frames: "
        <> Text.pack (show statsOriginalFrameCount)
        <> "\n  Retained frames: "
        <> Text.pack (show statsRetainedFrameCount)
        <> "\n  Elided frames: "
        <> Text.pack (show statsElidedFrameCount)
        <> "\n  Boundaries: "
        <> Text.pack (show statsBoundaryCount)
        <> "\n  Reduction: "
        <> Text.pack (show (round statsReductionPercent :: Int))
        <> "%"

-- | Legacy API: Render a single frame (public API)
renderFrame :: StackFrame -> Text
renderFrame StackFrame{..} =
    let path = fromMaybe "<unknown>" frameFilePath
        loc = case frameSourceLocation of
            Just SourceLocation{..} ->
                ":"
                    <> Text.pack (show locationLine)
                    <> ":"
                    <> Text.pack (show locationColumn)
            Nothing -> ""
        func = case frameFunctionName of
            Just fn -> " in " <> fn
            Nothing -> ""
     in "  --> " <> path <> loc <> func

-- | Legacy API: Render a single boundary (public API)
renderBoundary :: Boundary -> Text
renderBoundary Boundary{..} =
    "  Frame "
        <> Text.pack (show boundaryFrameIndex)
        <> ": "
        <> renderTransition boundaryTransition

-- | Legacy API: Render a single hint (public API)
renderHint :: DiagnosticHint -> Text
renderHint DiagnosticHint{..} =
    "hint: " <> hintMessage

-- | Legacy API: Render statistics (public API)
renderStatistics :: FilterStatistics -> Text
renderStatistics = renderStatisticsText

-- | Render as JSON (stub)
renderJson :: FilteredTrace -> Text
renderJson _ = "{\"error\": \"json rendering not implemented\"}"

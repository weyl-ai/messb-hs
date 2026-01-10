{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MessB.Hints (
    generateHints,
    generateHintsForError,
    generateHintsForFrames,
    hintPackageAsModule,
    hintMissingAttribute,
    hintInfiniteRecursionModule,
    hintTypeMismatch,
    PatternSignature (..),
    matchPatternSignature,
) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import MessB.Classification
import MessB.Types

-- | Pattern signature for known failure modes
data PatternSignature = PatternSignature
    { signatureErrorCategory :: ErrorCategory
    , signatureRequiredFunctions :: [Text]
    , signatureRequiredMessages :: [Text]
    , signatureHint :: DiagnosticHint
    }
    deriving stock (Eq, Show)

-- | Generate all applicable hints for a trace
generateHints ::
    ClassificationOntology ->
    StackTrace ->
    [DiagnosticHint]
generateHints ontology trace@StackTrace{..} =
    let categoryHints = generateHintsForError traceErrorCategory traceFrames
        frameHints = generateHintsForFrames ontology traceFrames
        patternHints = mapMaybe (matchPatternSignature trace) knownPatterns
     in deduplicateHints $ categoryHints ++ frameHints ++ patternHints

-- | Generate hints based on error category
generateHintsForError :: ErrorCategory -> [StackFrame] -> [DiagnosticHint]
generateHintsForError category frames = case category of
    InfiniteRecursion ->
        if hasStdenvInModuleContext frames
            then [hintPackageAsModule]
            else [hintInfiniteRecursionModule]
    AttributeMissing ->
        [hintMissingAttribute (extractMissingAttributeName frames)]
    TypeMismatch ->
        [hintTypeMismatch]
    AssertionFailed ->
        [hintAssertionFailed]
    ImportFailed ->
        [hintImportFailed]
    SyntaxError ->
        [hintSyntaxError]
    EvaluationError ->
        []
    UnknownError ->
        []

-- | Generate hints based on frame patterns
generateHintsForFrames ::
    ClassificationOntology ->
    [StackFrame] ->
    [DiagnosticHint]
generateHintsForFrames ontology frames =
    let classifications = map (fst . classifyStackFrame ontology) frames
        patterns = zip frames classifications
     in catMaybes
            [ detectCallPackageInModule patterns
            , detectOverlayRecursion patterns
            , detectMissingArgument patterns
            ]

-- | Check if stdenv appears in module evaluation context
hasStdenvInModuleContext :: [StackFrame] -> Bool
hasStdenvInModuleContext frames =
    any hasStdenvMessage frames && any hasModuleMessage frames
  where
    hasStdenvMessage frame = case frameMessage frame of
        Just message -> "stdenv" `Text.isInfixOf` message
        Nothing -> False
    hasModuleMessage frame = case frameMessage frame of
        Just message ->
            "module" `Text.isInfixOf` Text.toLower message
                || "evalModules" `Text.isInfixOf` message
        Nothing -> False

-- | Extract missing attribute name from frames
extractMissingAttributeName :: [StackFrame] -> Maybe Text
extractMissingAttributeName frames =
    let messages = mapMaybe frameMessage frames
        candidates = mapMaybe extractAttribute messages
     in case candidates of
            (attributeName : _) -> Just attributeName
            [] -> Nothing
  where
    extractAttribute message =
        let afterAttribute = Text.breakOn "attribute '" message
         in case afterAttribute of
                (_, rest)
                    | not (Text.null rest) ->
                        let afterQuote = Text.drop 11 rest -- "attribute '"
                            attributeName = Text.takeWhile (/= '\'') afterQuote
                         in if Text.null attributeName then Nothing else Just attributeName
                _ -> Nothing

-- | Detect callPackage being used in module context
detectCallPackageInModule ::
    [(StackFrame, FrameClassification)] ->
    Maybe DiagnosticHint
detectCallPackageInModule patterns =
    let hasCallPackage = any (isCallPackageFrame . fst) patterns
        hasModuleContext = any ((== ModuleSystem) . snd) patterns
     in if hasCallPackage && hasModuleContext
            then Just hintPackageAsModule
            else Nothing
  where
    isCallPackageFrame frame = case frameFunctionName frame of
        Just functionName -> "callPackage" `Text.isInfixOf` functionName
        Nothing -> False

-- | Detect overlay recursion
detectOverlayRecursion ::
    [(StackFrame, FrameClassification)] ->
    Maybe DiagnosticHint
detectOverlayRecursion patterns =
    let overlayFrames = filter ((== Overlay) . snd) patterns
        fixedPointFrames = filter ((== FixedPoint) . snd) patterns
     in if length overlayFrames > 3 && length fixedPointFrames > 2
            then Just hintOverlayRecursion
            else Nothing

-- | Detect missing argument pattern
detectMissingArgument ::
    [(StackFrame, FrameClassification)] ->
    Maybe DiagnosticHint
detectMissingArgument patterns =
    let messages = mapMaybe (frameMessage . fst) patterns
        hasMissingArg = any ("argument" `Text.isInfixOf`) messages
     in if hasMissingArg
            then Just hintMissingArgument
            else Nothing

-- | Known pattern signatures
knownPatterns :: [PatternSignature]
knownPatterns =
    [ PatternSignature
        { signatureErrorCategory = InfiniteRecursion
        , signatureRequiredFunctions = ["evalModules"]
        , signatureRequiredMessages = ["stdenv"]
        , signatureHint = hintPackageAsModule
        }
    , PatternSignature
        { signatureErrorCategory = InfiniteRecursion
        , signatureRequiredFunctions = ["fix", "extends"]
        , signatureRequiredMessages = []
        , signatureHint = hintOverlayRecursion
        }
    , PatternSignature
        { signatureErrorCategory = AttributeMissing
        , signatureRequiredFunctions = ["callPackage"]
        , signatureRequiredMessages = []
        , signatureHint = hintMissingDependency
        }
    ]

-- | Match a pattern signature against a trace
matchPatternSignature :: StackTrace -> PatternSignature -> Maybe DiagnosticHint
matchPatternSignature StackTrace{..} PatternSignature{..} =
    let categoryMatches = traceErrorCategory == signatureErrorCategory
        functionNames = mapMaybe frameFunctionName traceFrames
        functionsMatch = all (`elem` functionNames) signatureRequiredFunctions
        messages = mapMaybe frameMessage traceFrames
        allMessages = Text.unlines messages
        messagesMatch = all (`Text.isInfixOf` allMessages) signatureRequiredMessages
     in if categoryMatches && functionsMatch && messagesMatch
            then Just signatureHint
            else Nothing

-- | Remove duplicate hints
deduplicateHints :: [DiagnosticHint] -> [DiagnosticHint]
deduplicateHints = go []
  where
    go seen [] = reverse seen
    go seen (hint : rest)
        | hintMessage hint `elem` map hintMessage seen = go seen rest
        | otherwise = go (hint : seen) rest

-- Standard hints

hintPackageAsModule :: DiagnosticHint
hintPackageAsModule =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "a package derivation may have been imported as a module"
        , hintExplanation =
            Just
                "modules don't take 'stdenv' arguments - packages do. \
                \check that imports = [ ... ] contains module definitions, not packages."
        , hintSuggestedFix =
            Just
                "use pkgs.callPackage instead of imports for packages"
        }

hintInfiniteRecursionModule :: DiagnosticHint
hintInfiniteRecursionModule =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "infinite recursion in module evaluation"
        , hintExplanation =
            Just
                "a module option may reference itself, or two modules may \
                \have circular dependencies through their options."
        , hintSuggestedFix =
            Just
                "use mkIf or mkMerge to break the dependency cycle"
        }

hintMissingAttribute :: Maybe Text -> DiagnosticHint
hintMissingAttribute maybeAttributeName =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = case maybeAttributeName of
            Just attributeName -> "attribute '" <> attributeName <> "' not found"
            Nothing -> "attribute not found in set"
        , hintExplanation =
            Just
                "the attribute may be misspelled, or the set may not contain it. \
                \check nixpkgs for the correct attribute path."
        , hintSuggestedFix = Nothing
        }

hintTypeMismatch :: DiagnosticHint
hintTypeMismatch =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "type mismatch in expression"
        , hintExplanation =
            Just
                "nix expected a different type than what was provided. \
                \common causes: passing a string where a path is expected, \
                \or a set where a function is expected."
        , hintSuggestedFix = Nothing
        }

hintAssertionFailed :: DiagnosticHint
hintAssertionFailed =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "assertion failed"
        , hintExplanation =
            Just
                "a package or module assertion evaluated to false. \
                \this often indicates missing or incompatible dependencies."
        , hintSuggestedFix = Nothing
        }

hintImportFailed :: DiagnosticHint
hintImportFailed =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "import failed - file not found"
        , hintExplanation =
            Just
                "the imported file path does not exist. check for typos \
                \and ensure the file is tracked in git if using flakes."
        , hintSuggestedFix = Nothing
        }

hintSyntaxError :: DiagnosticHint
hintSyntaxError =
    DiagnosticHint
        { hintSeverity = HintCritical
        , hintMessage = "syntax error in nix expression"
        , hintExplanation =
            Just
                "the nix parser encountered unexpected tokens. \
                \common causes: missing semicolons, unbalanced braces, \
                \or incorrect string interpolation."
        , hintSuggestedFix = Nothing
        }

hintOverlayRecursion :: DiagnosticHint
hintOverlayRecursion =
    DiagnosticHint
        { hintSeverity = HintWarning
        , hintMessage = "possible overlay recursion detected"
        , hintExplanation =
            Just
                "overlays referencing each other can create infinite recursion. \
                \use 'final' and 'prev' carefully to avoid cycles."
        , hintSuggestedFix =
            Just
                "ensure overlays use 'prev' for dependencies, not 'final'"
        }

hintMissingDependency :: DiagnosticHint
hintMissingDependency =
    DiagnosticHint
        { hintSeverity = HintWarning
        , hintMessage = "possible missing dependency"
        , hintExplanation =
            Just
                "callPackage may be missing an argument. check the package's \
                \default.nix for required inputs."
        , hintSuggestedFix = Nothing
        }

hintMissingArgument :: DiagnosticHint
hintMissingArgument =
    DiagnosticHint
        { hintSeverity = HintWarning
        , hintMessage = "function called with missing argument"
        , hintExplanation =
            Just
                "a function expected an argument that wasn't provided."
        , hintSuggestedFix = Nothing
        }

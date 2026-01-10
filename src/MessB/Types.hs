{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MessB.Types (
    FrameClassification (..),
    ConfidenceLevel (..),
    StackFrame (..),
    StackTrace (..),
    FilteredTrace (..),
    FilterStatistics (..),
    Boundary (..),
    BoundaryTransition (..),
    DiagnosticHint (..),
    HintSeverity (..),
    ErrorCategory (..),
    ClassificationOntology (..),
    FilterConfiguration (..),
    FilterResult (..),
    SourceLocation (..),
    PathPatterns (..),
    emptyStackFrame,
    emptyFilterConfiguration,
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Classification of stack frames by origin
data FrameClassification
    = UserCode
    | ModuleSystem
    | LibraryInternal
    | PackageSet
    | Overlay
    | FixedPoint
    | Unknown
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Confidence in a classification decision
data ConfidenceLevel
    = HighConfidence
    | MediumConfidence
    | LowConfidence
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Source location within a file
data SourceLocation = SourceLocation
    { locationFilePath :: Text
    , locationLine :: Int
    , locationColumn :: Int
    }
    deriving stock (Eq, Show, Generic)

-- | A single frame in a stack trace
data StackFrame = StackFrame
    { frameFilePath :: Maybe Text
    , frameFunctionName :: Maybe Text
    , frameMessage :: Maybe Text
    , frameSourceLocation :: Maybe SourceLocation
    , frameExplicitRole :: Maybe FrameClassification
    , frameRawText :: Text
    }
    deriving stock (Eq, Show, Generic)

-- | Smart constructor for empty frame
emptyStackFrame :: StackFrame
emptyStackFrame =
    StackFrame
        { frameFilePath = Nothing
        , frameFunctionName = Nothing
        , frameMessage = Nothing
        , frameSourceLocation = Nothing
        , frameExplicitRole = Nothing
        , frameRawText = ""
        }

-- | Complete stack trace from Nix evaluator
data StackTrace = StackTrace
    { traceErrorMessage :: Text
    , traceErrorCategory :: ErrorCategory
    , traceFrames :: [StackFrame]
    , traceRawOutput :: Text
    }
    deriving stock (Eq, Show, Generic)

-- | Boundary between code regions
data BoundaryTransition
    = ModuleSystemToUserCode
    | UserCodeToModuleSystem
    | LibraryToUserCode
    | UserCodeToLibrary
    | OverlayToPackageSet
    | PackageSetToOverlay
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | A detected boundary in the trace
data Boundary = Boundary
    { boundaryFrameIndex :: Int
    , boundaryTransition :: BoundaryTransition
    , boundaryConfidence :: ConfidenceLevel
    }
    deriving stock (Eq, Show, Generic)

-- | Severity of diagnostic hints
data HintSeverity
    = HintCritical
    | HintWarning
    | HintInfo
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Diagnostic hint for the user
data DiagnosticHint = DiagnosticHint
    { hintSeverity :: HintSeverity
    , hintMessage :: Text
    , hintExplanation :: Maybe Text
    , hintSuggestedFix :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

-- | Category of Nix evaluation errors
data ErrorCategory
    = InfiniteRecursion
    | AttributeMissing
    | TypeMismatch
    | AssertionFailed
    | ImportFailed
    | SyntaxError
    | EvaluationError
    | UnknownError
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Mapping from function names to classifications
newtype ClassificationOntology = ClassificationOntology
    {unOntology :: Map Text FrameClassification}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Semigroup, Monoid)

-- | Configuration for the filtering process
data FilterConfiguration = FilterConfiguration
    { configMaxFrames :: Int
    , configPreserveBoundaries :: Bool
    , configIncludeHints :: Bool
    , configOntology :: ClassificationOntology
    , configPathPatterns :: PathPatterns
    }
    deriving stock (Eq, Show, Generic)

-- | Path patterns for classification heuristics
data PathPatterns = PathPatterns
    { patternsUserCode :: [Text]
    , patternsLibraryInternal :: [Text]
    , patternsModuleSystem :: [Text]
    , patternsFlakeSource :: [Text]
    }
    deriving stock (Eq, Show, Generic)

-- | Default configuration
emptyFilterConfiguration :: FilterConfiguration
emptyFilterConfiguration =
    FilterConfiguration
        { configMaxFrames = 10
        , configPreserveBoundaries = True
        , configIncludeHints = True
        , configOntology = mempty
        , configPathPatterns =
            PathPatterns
                { patternsUserCode = ["/home/", "/etc/nixos/", "/etc/darwin/"]
                , patternsLibraryInternal = ["/nix/store/"]
                , patternsModuleSystem = ["lib/modules.nix", "lib/options.nix"]
                , patternsFlakeSource = ["-source/"]
                }
        }

-- | Result of filtering a single frame
data FilterResult
    = FrameRetained StackFrame FrameClassification ConfidenceLevel
    | FrameElided StackFrame
    | FrameBoundary StackFrame Boundary
    deriving stock (Eq, Show, Generic)

-- | A filtered and annotated trace
data FilteredTrace = FilteredTrace
    { filteredErrorMessage :: Text
    , filteredErrorCategory :: ErrorCategory
    , filteredFrames :: [FilterResult]
    , filteredBoundaries :: [Boundary]
    , filteredHints :: [DiagnosticHint]
    , filteredStatistics :: FilterStatistics
    }
    deriving stock (Eq, Show, Generic)

-- | Statistics about the filtering process
data FilterStatistics = FilterStatistics
    { statsOriginalFrameCount :: Int
    , statsRetainedFrameCount :: Int
    , statsElidedFrameCount :: Int
    , statsBoundaryCount :: Int
    , statsReductionPercent :: Double
    }
    deriving stock (Eq, Show, Generic)

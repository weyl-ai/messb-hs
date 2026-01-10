{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MessB (
    -- * Core filtering
    filterStackTrace,
    filterStackTraceWithConfig,
    parseAndFilter,

    -- * Types
    module MessB.Types,

    -- * Classification
    classifyStackFrame,
    buildDefaultOntology,

    -- * Parsing
    parseStackTrace,
    parseStackFrame,

    -- * Rendering
    renderFilteredTrace,
    renderCompact,
    RenderFormat (..),

    -- * Configuration
    defaultConfiguration,
) where

import Data.Text (Text)

import MessB.Classification (buildDefaultOntology, classifyStackFrame)
import MessB.Filter (filterTrace)
import MessB.Parser (parseStackFrame, parseStackTrace)
import MessB.Render (RenderFormat (..), renderCompact, renderFilteredTrace)
import MessB.Types

-- | Filter a stack trace with default configuration
filterStackTrace :: StackTrace -> FilteredTrace
filterStackTrace = filterTrace defaultConfiguration

-- | Filter a stack trace with custom configuration
filterStackTraceWithConfig :: FilterConfiguration -> StackTrace -> FilteredTrace
filterStackTraceWithConfig = filterTrace

-- | Parse raw output and filter in one step
parseAndFilter :: Text -> Either Text FilteredTrace
parseAndFilter rawOutput = do
    trace <- parseStackTrace rawOutput
    Right $ filterStackTrace trace

-- | Default configuration for filtering
defaultConfiguration :: FilterConfiguration
defaultConfiguration =
    FilterConfiguration
        { configMaxFrames = 10
        , configPreserveBoundaries = True
        , configIncludeHints = True
        , configOntology = buildDefaultOntology
        , configPathPatterns =
            PathPatterns
                { patternsUserCode =
                    [ "/home/"
                    , "/etc/nixos/"
                    , "/etc/darwin/"
                    , "/Users/"
                    ]
                , patternsLibraryInternal =
                    [ "/nix/store/"
                    ]
                , patternsModuleSystem =
                    [ "lib/modules.nix"
                    , "lib/options.nix"
                    , "lib/types.nix"
                    ]
                , patternsFlakeSource =
                    [ "-source/"
                    ]
                }
        }

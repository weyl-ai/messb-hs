{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (stderr)

import MessB

data Options = Options
    { optionsInputFile :: Maybe FilePath
    , optionsMaxFrames :: Int
    , optionsFormat :: RenderFormat
    , optionsNoHints :: Bool
    , optionsVerbose :: Bool
    }
    deriving stock (Show)

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optional
            ( strOption
                ( long "input"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "Input file (stdin if not specified)"
                )
            )
        <*> option
            auto
            ( long "max-frames"
                <> short 'n'
                <> metavar "N"
                <> value 10
                <> showDefault
                <> help "Maximum frames to retain"
            )
        <*> (formatOption <|> pure FormatCompact)
        <*> switch
            ( long "no-hints"
                <> help "Disable diagnostic hints"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output"
            )

formatOption :: Parser RenderFormat
formatOption =
    flag' FormatCompact (long "compact" <> help "Compact output (default)")
        <|> flag' FormatVerbose (long "verbose-format" <> help "Verbose output format")

optionsInfo :: ParserInfo Options
optionsInfo =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc "Filter Nix stack traces to their causal essence"
            <> header "mess-b - Minimal Explanatory Stack Subtrace with Boundaries"
        )

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    inputText <- maybe TextIO.getContents TextIO.readFile optionsInputFile

    let configuration =
            defaultConfiguration
                { configMaxFrames = optionsMaxFrames
                , configIncludeHints = not optionsNoHints
                }

    let format = if optionsVerbose then FormatVerbose else optionsFormat

    case parseAndFilterWithConfig configuration inputText of
        Left errorMessage -> do
            TextIO.hPutStrLn stderr $ "error: " <> errorMessage
            exitFailure
        Right filtered -> do
            output <- renderFilteredTrace format filtered
            TextIO.putStrLn output

-- | Parse and filter with custom configuration
parseAndFilterWithConfig :: FilterConfiguration -> Text -> Either Text FilteredTrace
parseAndFilterWithConfig config rawOutput = do
    trace <- parseStackTrace rawOutput
    Right $ filterStackTraceWithConfig config trace

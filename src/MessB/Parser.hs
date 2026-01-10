{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MessB.Parser (
    parseStackTrace,
    parseStackFrame,
    parseErrorCategory,
    parseSourceLocation,
    extractFramesFromRaw,
) where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

import MessB.Types

-- | Parse a complete stack trace from raw Nix evaluator output
parseStackTrace :: Text -> Either Text StackTrace
parseStackTrace rawOutput = do
    let textLines = Text.lines rawOutput
    errorMessage <- extractErrorMessage textLines
    let errorCategory = parseErrorCategory errorMessage
    let frames = extractFramesFromRaw rawOutput
    Right
        StackTrace
            { traceErrorMessage = errorMessage
            , traceErrorCategory = errorCategory
            , traceFrames = frames
            , traceRawOutput = rawOutput
            }

-- | Extract the error message from the first line(s)
extractErrorMessage :: [Text] -> Either Text Text
extractErrorMessage [] = Left "empty trace"
extractErrorMessage allLines =
    -- Look for lines with "error:" and extract the message after it
    let errorLines = filter (Text.isInfixOf "error:") allLines
        -- Get the last error line with actual content after "error:"
        messagesWithContent = mapMaybe extractFromErrorLine errorLines
     in case messagesWithContent of
            (msg : _) -> Right msg -- Take first non-empty message
            [] -> Right "" -- No error message found
  where
    extractFromErrorLine line =
        let stripped = Text.strip line
         in case Text.stripPrefix "error:" stripped of
                Just afterError ->
                    let msg = Text.strip afterError
                     in if Text.null msg then Nothing else Just msg
                Nothing -> Nothing

-- | Determine error category from message
parseErrorCategory :: Text -> ErrorCategory
parseErrorCategory message
    | "infinite recursion" `Text.isInfixOf` messageLower = InfiniteRecursion
    | "stack overflow" `Text.isInfixOf` messageLower = InfiniteRecursion -- Treat as recursion
    | "attribute" `Text.isInfixOf` messageLower
        && "missing" `Text.isInfixOf` messageLower =
        AttributeMissing
    | "is not a" `Text.isInfixOf` messageLower = TypeMismatch
    | "is a" `Text.isInfixOf` messageLower
        && "expected" `Text.isInfixOf` messageLower =
        TypeMismatch -- "is a X while Y was expected"
    | "type" `Text.isInfixOf` messageLower
        && "expected" `Text.isInfixOf` messageLower =
        TypeMismatch
    | "cannot coerce" `Text.isInfixOf` messageLower = TypeMismatch
    | "assertion" `Text.isInfixOf` messageLower
        && "failed" `Text.isInfixOf` messageLower =
        AssertionFailed
    | "abort" `Text.isInfixOf` messageLower = EvaluationError
    | "cannot find" `Text.isInfixOf` messageLower = ImportFailed
    | "file not found" `Text.isInfixOf` messageLower = ImportFailed
    | "syntax error" `Text.isInfixOf` messageLower = SyntaxError
    | "unexpected" `Text.isInfixOf` messageLower = SyntaxError
    | "evaluation error" `Text.isInfixOf` messageLower = EvaluationError
    | otherwise = UnknownError
  where
    messageLower = Text.toLower message

-- | Extract all frames from raw trace output
extractFramesFromRaw :: Text -> [StackFrame]
extractFramesFromRaw rawOutput =
    let textLines = Text.lines rawOutput
        frameChunks = splitIntoFrameChunks textLines
     in map parseFrameChunk frameChunks

-- | Split lines into chunks, each representing one frame
splitIntoFrameChunks :: [Text] -> [[Text]]
splitIntoFrameChunks = go [] []
  where
    go accumulated current [] =
        reverse $ if null current then accumulated else reverse current : accumulated
    go accumulated current (textLine : rest)
        | isFrameStart textLine =
            let newAccumulated =
                    if null current
                        then accumulated
                        else reverse current : accumulated
             in go newAccumulated [textLine] rest
        | otherwise =
            go accumulated (textLine : current) rest

    isFrameStart textLine =
        "at " `Text.isPrefixOf` Text.stripStart textLine
            || "... " `Text.isPrefixOf` Text.stripStart textLine
            || "-->" `Text.isInfixOf` textLine

-- | Parse a chunk of lines into a StackFrame
parseFrameChunk :: [Text] -> StackFrame
parseFrameChunk [] = emptyStackFrame
parseFrameChunk chunkLines@(firstLine : _) =
    let rawText = Text.unlines chunkLines
        sourceLocation = parseSourceLocation firstLine
        functionName = extractFunctionName chunkLines
        message = extractMessage chunkLines
        filePath = locationFilePath <$> sourceLocation
     in StackFrame
            { frameFilePath = filePath
            , frameFunctionName = functionName
            , frameMessage = message
            , frameSourceLocation = sourceLocation
            , frameExplicitRole = Nothing
            , frameRawText = rawText
            }

-- | Parse source location from a frame line
parseSourceLocation :: Text -> Maybe SourceLocation
parseSourceLocation textLine =
    parseAtLocation textLine <|> parseArrowLocation textLine

-- | Parse "at /path/to/file.nix:123:45:" format
parseAtLocation :: Text -> Maybe SourceLocation
parseAtLocation textLine = do
    let stripped = Text.strip textLine
    rest <- Text.stripPrefix "at " stripped
    let colonSplit = Text.splitOn ":" rest
    case colonSplit of
        (pathPart : lineNumText : colNumText : _) -> do
            lineNumber <- readMaybe (Text.unpack lineNumText)
            columnNumber <- readMaybe (Text.unpack $ Text.takeWhile isDigit colNumText)
            Just
                SourceLocation
                    { locationFilePath = pathPart
                    , locationLine = lineNumber
                    , locationColumn = columnNumber
                    }
        _ -> Nothing

-- | Parse "--> /path/to/file.nix:123:45" format
parseArrowLocation :: Text -> Maybe SourceLocation
parseArrowLocation textLine = do
    let stripped = Text.strip textLine
    rest <- Text.stripPrefix "-->" stripped <|> Text.stripPrefix "  -->" stripped
    let pathText = Text.strip rest
    let colonSplit = Text.splitOn ":" pathText
    case colonSplit of
        (pathPart : lineNumText : colNumText : _) -> do
            lineNumber <- readMaybe (Text.unpack lineNumText)
            columnNumber <- readMaybe (Text.unpack $ Text.takeWhile isDigit colNumText)
            Just
                SourceLocation
                    { locationFilePath = pathPart
                    , locationLine = lineNumber
                    , locationColumn = columnNumber
                    }
        (pathPart : lineNumText : _) -> do
            lineNumber <- readMaybe (Text.unpack $ Text.takeWhile isDigit lineNumText)
            Just
                SourceLocation
                    { locationFilePath = pathPart
                    , locationLine = lineNumber
                    , locationColumn = 0
                    }
        _ -> Nothing

-- | Extract function name from frame lines
extractFunctionName :: [Text] -> Maybe Text
extractFunctionName chunkLines =
    let candidates = mapMaybe extractFromLine chunkLines
     in case candidates of
            (functionName : _) -> Just functionName
            [] -> Nothing
  where
    extractFromLine textLine
        | "while evaluating" `Text.isInfixOf` textLine =
            extractQuotedIdentifier textLine
        | "while calling" `Text.isInfixOf` textLine =
            extractQuotedIdentifier textLine
        | otherwise = Nothing

    extractQuotedIdentifier textLine =
        let afterQuote = Text.dropWhile (/= '\'') textLine
         in case Text.uncons afterQuote of
                Just ('\'', rest) ->
                    let identifier = Text.takeWhile (/= '\'') rest
                     in if Text.null identifier then Nothing else Just identifier
                _ -> extractUnquotedIdentifier textLine

    extractUnquotedIdentifier textLine =
        let wordList = Text.words textLine
            candidates = filter isLikelyIdentifier wordList
         in case candidates of
                (identifier : _) -> Just identifier
                [] -> Nothing

    isLikelyIdentifier word =
        not (Text.null word)
            && Text.all isIdentifierChar word
            && notElem word reservedWords

    isIdentifierChar c = c == '_' || c == '.' || c == '-' || isAlphaNum c
    isAlphaNum c =
        isAsciiLower c
            || isAsciiUpper c
            || isDigit c

    reservedWords =
        [ "while"
        , "evaluating"
        , "the"
        , "attribute"
        , "argument"
        , "module"
        , "option"
        , "in"
        , "at"
        , "from"
        , "to"
        ]

-- | Extract message from frame lines
extractMessage :: [Text] -> Maybe Text
extractMessage chunkLines =
    case filter isMessageLine chunkLines of
        (messageLine : _) -> Just $ Text.strip messageLine
        [] -> Nothing
  where
    isMessageLine textLine =
        "while " `Text.isPrefixOf` Text.stripStart textLine
            || "... " `Text.isPrefixOf` Text.stripStart textLine

-- | Parse a single stack frame from text
parseStackFrame :: Text -> StackFrame
parseStackFrame rawText = parseFrameChunk (Text.lines rawText)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs

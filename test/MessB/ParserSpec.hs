{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MessB.ParserSpec (spec) where

import Data.Text (Text)
import Data.Text qualified
import Test.Hspec

import MessB.Parser
import MessB.Types

spec :: Spec
spec = describe "MessB.Parser" $ do
    describe "parseErrorCategory" $ do
        it "detects infinite recursion" $ do
            parseErrorCategory "infinite recursion encountered"
                `shouldBe` InfiniteRecursion

        it "detects missing attribute" $ do
            parseErrorCategory "attribute 'foo' missing"
                `shouldBe` AttributeMissing

        it "detects type mismatch" $ do
            parseErrorCategory "value is a string while a set was expected"
                `shouldBe` TypeMismatch

        it "detects assertion failed" $ do
            parseErrorCategory "assertion 'lib.assertMsg' failed"
                `shouldBe` AssertionFailed

        it "detects import failed" $ do
            parseErrorCategory "cannot find 'foo.nix'"
                `shouldBe` ImportFailed

        it "detects syntax error" $ do
            parseErrorCategory "syntax error, unexpected '}'"
                `shouldBe` SyntaxError

        it "returns UnknownError for unrecognized messages" $ do
            parseErrorCategory "something weird happened"
                `shouldBe` UnknownError

    describe "parseSourceLocation" $ do
        it "parses 'at' format" $ do
            let result = parseSourceLocation "at /path/to/file.nix:42:10:"
            result `shouldBe` Just (SourceLocation "/path/to/file.nix" 42 10)

        it "parses arrow format" $ do
            let result = parseSourceLocation "  --> /path/to/file.nix:123:5"
            result `shouldBe` Just (SourceLocation "/path/to/file.nix" 123 5)

        it "returns Nothing for invalid format" $ do
            let result = parseSourceLocation "not a location"
            result `shouldBe` Nothing

    describe "parseStackTrace" $ do
        it "parses a simple trace" $ do
            let rawTrace = sampleInfiniteRecursionTrace
            case parseStackTrace rawTrace of
                Left errorMessage -> expectationFailure $ "Parse failed: " <> show errorMessage
                Right trace -> do
                    traceErrorCategory trace `shouldBe` InfiniteRecursion
                    length (traceFrames trace) `shouldSatisfy` (> 0)

        it "extracts error message" $ do
            let rawTrace = "error: infinite recursion encountered\n  at /file.nix:1:1:\n"
            case parseStackTrace rawTrace of
                Left errorMessage -> expectationFailure $ "Parse failed: " <> show errorMessage
                Right trace ->
                    traceErrorMessage trace `shouldBe` "infinite recursion encountered"

    describe "parseStackFrame" $ do
        it "extracts file path from frame" $ do
            let rawFrame = "at /home/user/config.nix:10:5:\n  ... while evaluating"
            let frame = parseStackFrame rawFrame
            frameFilePath frame `shouldBe` Just "/home/user/config.nix"

        it "extracts message from frame" $ do
            let rawFrame = "at /file.nix:1:1:\n  ... while evaluating the module argument 'stdenv'"
            let frame = parseStackFrame rawFrame
            frameMessage frame `shouldSatisfy` \case
                Just message -> "stdenv" `Data.Text.isInfixOf` message
                Nothing -> False

-- Sample traces for testing

sampleInfiniteRecursionTrace :: Text
sampleInfiniteRecursionTrace =
    "error: infinite recursion encountered\n\
    \\n\
    \       at /nix/store/abc-source/lib/modules.nix:140:9:\n\
    \          139| in warnDeprecation opt //\n\
    \          140|     { value = addErrorContext \"while evaluating...\"\n\
    \             |          ^\n\
    \\n\
    \       ... while evaluating the attribute 'value'\n\
    \\n\
    \       at /nix/store/abc-source/lib/modules.nix:809:9:\n\
    \          808|     in {\n\
    \          809|       value = builtins.addErrorContext \"while evaluating...\"\n\
    \             |          ^\n\
    \\n\
    \       ... while evaluating the module argument 'stdenv'\n\
    \\n\
    \       at /nix/store/abc-source/nixos/common.nix:109:20\n"

-- TODO: Use in additional test cases
_sampleMissingAttributeTrace :: Text
_sampleMissingAttributeTrace =
    "error: attribute 'foo' missing\n\
    \\n\
    \       at /home/user/config.nix:15:3:\n\
    \          14| {\n\
    \          15|   pkgs.foo\n\
    \             |   ^\n\
    \\n\
    \       ... while evaluating 'foo'\n"

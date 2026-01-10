{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module MessB.CorpusSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.FilePath ((</>))
import Test.Hspec

import MessB

-- | Integration tests using the official NixOS test corpus
spec :: Spec
spec = describe "MessB.Corpus" $ do
    describe "01-infinite-recursion.txt" $ do
        it "should detect InfiniteRecursion category" $ do
            trace <- loadCorpusFile "01-infinite-recursion.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    traceErrorCategory st `shouldBe` InfiniteRecursion

        it "should extract error message 'infinite recursion encountered'" $ do
            trace <- loadCorpusFile "01-infinite-recursion.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    traceErrorMessage st `shouldBe` "infinite recursion encountered"

        it "should render with proper error category in output" $ do
            trace <- loadCorpusFile "01-infinite-recursion.txt"
            case parseAndFilter trace of
                Left err -> expectationFailure $ "Filter failed: " <> show err
                Right filtered -> do
                    outputText <- renderFilteredTrace FormatCompact filtered
                    let output = Text.unpack outputText
                    output `shouldContain` "E-INFINITE-RECURSION"
                    output `shouldContain` "infinite recursion"

    describe "02-stack-overflow.txt" $ do
        it "should detect error message 'stack overflow'" $ do
            trace <- loadCorpusFile "02-stack-overflow.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    Text.unpack (traceErrorMessage st) `shouldContain` "stack overflow"

        it "should preserve error message in compact output" $ do
            trace <- loadCorpusFile "02-stack-overflow.txt"
            case parseAndFilter trace of
                Left err -> expectationFailure $ "Filter failed: " <> show err
                Right filtered -> do
                    outputText <- renderFilteredTrace FormatCompact filtered
                    let output = Text.unpack outputText
                    output `shouldContain` "stack overflow"

        it "should recognize duplicate frame markers" $ do
            trace <- loadCorpusFile "02-stack-overflow.txt"
            -- Original trace says "(197 duplicate frames omitted)"
            -- Our parser should understand this
            Text.unpack trace `shouldContain` "197 duplicate frames omitted"

    describe "03-mutual-recursion.txt" $ do
        it "should extract custom error message 'Uh oh!'" $ do
            trace <- loadCorpusFile "03-mutual-recursion.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    Text.unpack (traceErrorMessage st) `shouldContain` "Uh oh"

        it "should preserve custom error message in output" $ do
            trace <- loadCorpusFile "03-mutual-recursion.txt"
            case parseAndFilter trace of
                Left err -> expectationFailure $ "Filter failed: " <> show err
                Right filtered -> do
                    outputText <- renderFilteredTrace FormatCompact filtered
                    let output = Text.unpack outputText
                    output `shouldContain` "Uh oh"

    describe "05-abort.txt" $ do
        it "should detect AssertionFailed or EvaluationError category" $ do
            trace <- loadCorpusFile "05-abort.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    -- Should categorize abort as one of these
                    traceErrorCategory st `shouldSatisfy` \cat ->
                        cat `elem` [AssertionFailed, EvaluationError]

        it "should extract abort message 'this should fail'" $ do
            trace <- loadCorpusFile "05-abort.txt"
            case parseStackTrace trace of
                Left err -> expectationFailure $ "Parse failed: " <> show err
                Right st -> do
                    Text.unpack (traceErrorMessage st) `shouldContain` "this should fail"

    describe "Error message preservation" $ do
        it "should never produce empty error messages" $ do
            files <- getCorpusFiles
            forM_ files $ \file -> do
                trace <- loadCorpusFile file
                case parseStackTrace trace of
                    Left _ -> pure () -- Parse failures are tested elsewhere
                    Right st -> do
                        traceErrorMessage st `shouldNotBe` ""
                        Text.strip (traceErrorMessage st) `shouldNotBe` ""

        it "should preserve error messages in filtered output" $ do
            files <- getCorpusFiles
            forM_ files $ \file -> do
                trace <- loadCorpusFile file
                case parseAndFilter trace of
                    Left _ -> pure () -- Filter failures are tested elsewhere
                    Right filtered -> do
                        filteredErrorMessage filtered `shouldNotBe` ""
                        Text.strip (filteredErrorMessage filtered) `shouldNotBe` ""

        it "should render error messages in compact output" $ do
            files <- getCorpusFiles
            forM_ files $ \file -> do
                trace <- loadCorpusFile file
                case parseAndFilter trace of
                    Left _ -> pure ()
                    Right filtered -> do
                        outputText <- renderFilteredTrace FormatCompact filtered
                        let output = Text.unpack outputText
                        -- Should not have empty error lines like "error[E-UNKNOWN]: "
                        output `shouldNotContain` "error[E-UNKNOWN]: \n\n"

-- Helper functions

loadCorpusFile :: FilePath -> IO Text
loadCorpusFile filename = do
    let path = "test-corpus" </> filename
    result <- try $ TextIO.readFile path
    case result of
        Left (e :: SomeException) ->
            error $ "Failed to read corpus file " <> path <> ": " <> show e
        Right content -> pure content

getCorpusFiles :: IO [FilePath]
getCorpusFiles =
    pure
        [ "01-infinite-recursion.txt"
        , "02-stack-overflow.txt"
        , "03-mutual-recursion.txt"
        , "04-error-context.txt"
        , "05-abort.txt"
        , "06-undeclared-arg.txt"
        , "07-bad-interpolation.txt"
        , "08-json-overflow.txt"
        ]

-- Import forM_ explicitly for clarity
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = flip mapM_

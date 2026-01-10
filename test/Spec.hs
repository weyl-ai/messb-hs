module Main (main) where

import Hedgehog (checkParallel)
import System.Exit (exitFailure, exitSuccess)
import Test.Hspec

import MessB.ClassificationSpec qualified

-- import MessB.CorpusSpec qualified
import MessB.FilterSpec qualified
import MessB.ParserSpec qualified
import MessB.Properties qualified

main :: IO ()
main = do
    -- Run HSpec tests
    hspec $ do
        MessB.ClassificationSpec.spec
        MessB.ParserSpec.spec
        MessB.FilterSpec.spec
    -- MessB.CorpusSpec.spec  -- Disabled: corpus files not included in repository

    -- Run Hedgehog property tests
    propertyResult <- checkParallel MessB.Properties.propertyTests
    if propertyResult
        then exitSuccess
        else exitFailure

{-# LANGUAGE OverloadedStrings #-}

module MessB.ClassificationSpec (spec) where

import Test.Hspec

import MessB.Classification
import MessB.Types

spec :: Spec
spec = describe "MessB.Classification" $ do
    describe "classifyStackFrame" $ do
        it "classifies user home paths as UserCode with high confidence" $ do
            let frame = emptyStackFrame{frameFilePath = Just "/home/user/config.nix"}
            let (classification, confidence) = classifyStackFrame mempty frame
            classification `shouldBe` UserCode
            confidence `shouldBe` HighConfidence

        it "classifies /etc/nixos/ paths as UserCode" $ do
            let frame = emptyStackFrame{frameFilePath = Just "/etc/nixos/configuration.nix"}
            let (classification, _) = classifyStackFrame mempty frame
            classification `shouldBe` UserCode

        it "classifies /etc/darwin/ paths as UserCode" $ do
            let frame = emptyStackFrame{frameFilePath = Just "/etc/darwin/configuration.nix"}
            let (classification, _) = classifyStackFrame mempty frame
            classification `shouldBe` UserCode

        it "classifies /nix/store/ paths as LibraryInternal" $ do
            let frame =
                    emptyStackFrame
                        { frameFilePath = Just "/nix/store/abc123-nixpkgs/lib/modules.nix"
                        }
            let (classification, confidence) = classifyStackFrame mempty frame
            classification `shouldBe` LibraryInternal
            confidence `shouldBe` MediumConfidence

        it "classifies flake source paths as UserCode" $ do
            let frame =
                    emptyStackFrame
                        { frameFilePath = Just "/nix/store/abc123-source/modules/web.nix"
                        }
            let (classification, confidence) = classifyStackFrame mempty frame
            classification `shouldBe` UserCode
            confidence `shouldBe` MediumConfidence

        it "classifies flake source with lib/ as LibraryInternal" $ do
            let frame =
                    emptyStackFrame
                        { frameFilePath = Just "/nix/store/abc123-source/lib/default.nix"
                        }
            let (classification, _) = classifyStackFrame mempty frame
            classification `shouldBe` LibraryInternal

        it "uses explicit role when present" $ do
            let frame =
                    emptyStackFrame
                        { frameFilePath = Just "/nix/store/abc123/something.nix"
                        , frameExplicitRole = Just UserCode
                        }
            let (classification, confidence) = classifyStackFrame mempty frame
            classification `shouldBe` UserCode
            confidence `shouldBe` HighConfidence

        it "uses ontology lookup when function name matches" $ do
            let ontology = buildDefaultOntology
            let frame = emptyStackFrame{frameFunctionName = Just "mergeModules"}
            let (classification, confidence) = classifyStackFrame ontology frame
            classification `shouldBe` ModuleSystem
            confidence `shouldBe` HighConfidence

        it "returns Unknown with low confidence for unrecognized paths" $ do
            let frame = emptyStackFrame{frameFilePath = Just "/weird/path/file.nix"}
            let (classification, confidence) = classifyStackFrame mempty frame
            classification `shouldBe` Unknown
            confidence `shouldBe` LowConfidence

        it "returns Unknown for empty frame" $ do
            let (classification, confidence) = classifyStackFrame mempty emptyStackFrame
            classification `shouldBe` Unknown
            confidence `shouldBe` LowConfidence

    describe "buildDefaultOntology" $ do
        it "contains module system functions" $ do
            let ontology = buildDefaultOntology
            let frame = emptyStackFrame{frameFunctionName = Just "evalModules"}
            let (classification, _) = classifyStackFrame ontology frame
            classification `shouldBe` ModuleSystem

        it "contains fixed point functions" $ do
            let ontology = buildDefaultOntology
            let frame = emptyStackFrame{frameFunctionName = Just "fix"}
            let (classification, _) = classifyStackFrame ontology frame
            classification `shouldBe` FixedPoint

        it "contains overlay functions" $ do
            let ontology = buildDefaultOntology
            let frame = emptyStackFrame{frameFunctionName = Just "applyOverlays"}
            let (classification, _) = classifyStackFrame ontology frame
            classification `shouldBe` Overlay

        it "contains package set functions" $ do
            let ontology = buildDefaultOntology
            let frame = emptyStackFrame{frameFunctionName = Just "callPackage"}
            let (classification, _) = classifyStackFrame ontology frame
            classification `shouldBe` PackageSet

    describe "isFlakeSourcePath" $ do
        it "recognizes -source/ pattern" $ do
            isFlakeSourcePath "/nix/store/abc-source/config.nix" `shouldBe` True

        it "rejects regular store paths" $ do
            isFlakeSourcePath "/nix/store/abc-nixpkgs/lib.nix" `shouldBe` False

    describe "isUserCodePath" $ do
        it "recognizes /home/ paths" $ do
            isUserCodePath "/home/user/file.nix" `shouldBe` True

        it "recognizes /etc/nixos/ paths" $ do
            isUserCodePath "/etc/nixos/configuration.nix" `shouldBe` True

        it "rejects /nix/store/ paths" $ do
            isUserCodePath "/nix/store/abc/file.nix" `shouldBe` False

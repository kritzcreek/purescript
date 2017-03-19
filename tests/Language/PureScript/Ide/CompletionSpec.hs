{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import           Protolude hiding (to, from, (&))
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Test as Test
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

moduleB :: (Text, [IdeDeclarationAnn])
moduleB = ("ModuleB",
           [ ideValue "sfValueB" Nothing
           , ideSynonym "SFTypeB" P.tyString
           , ideType "SFDataB" Nothing
           ])

moduleC :: (Text, [IdeDeclarationAnn])
moduleC = ("ModuleC",
           [ ideValue "sfValueB" Nothing `annExp` m "ModuleB"
           , ideSynonym "SFTypeC" P.tyString
           ])

moduleA :: (Text, [IdeDeclarationAnn])
moduleA = ("ModuleA",
           [ ideValue "sfValue" Nothing
           , ideSynonym "SFType" P.tyString
           , ideType "SFData" Nothing
           , ideDtor "SFOne" "SFData" Nothing
           , ideDtor "SFTwo" "SFData" Nothing
           , ideDtor "SFThree" "SFData" Nothing
           , ideTypeClass "SFClass" []
           , ideValueOp "<$>" (P.Qualified Nothing (Left "")) 0 Nothing Nothing
           , ideTypeOp "~>" (P.Qualified Nothing "") 0 Nothing Nothing
           ])
m :: Text -> P.ModuleName
m = P.moduleNameFromString

moduleBCompletions :: [Completion]
moduleBCompletions = map (\decl -> completionFromMatch $ Match (m "ModuleB", decl)) (snd moduleB)

getCompletions :: [Text] -> IO [Completion]
getCompletions modules = Test.inProject $ do
  ([Right (CompletionResult cs)], _) <-
    Test.runIde' Test.defConfig ideState [Complete [moduleFilter (map m modules)] mempty Nothing]
  pure cs
  where
    ideState = emptyIdeState `s3` [moduleA, moduleB, moduleC]

shouldBeEqualSorted :: (Show a, Ord a) => [a] -> [a] -> Expectation
shouldBeEqualSorted = shouldBe `on` sort

spec :: Spec
spec = do
  describe "Filtering by FilePath" $ do
    it "should filter by the imported modules" $ do
      cs <- getCompletions ["ModuleB"]
      cs `shouldBeEqualSorted` moduleBCompletions
    it "should omit reexported definitions that are already in scope" $ do
      cs <- getCompletions ["ModuleB", "ModuleC"]
      cs `shouldBeEqualSorted` (moduleBCompletions ++ [completionFromMatch (Match (m "ModuleC", ideSynonym "SFTypeC" P.tyString))])

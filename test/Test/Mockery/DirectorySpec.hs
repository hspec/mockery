
module Test.Mockery.DirectorySpec where

import           Test.Hspec

import           Test.Mockery.Directory

spec :: Spec
spec = do
  describe "withFile" $ do
    it "creates a temporary file with the given contents" $ do
      withFile "foo" $ \file ->
        readFile file `shouldReturn` "foo"

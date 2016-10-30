module Test.Mockery.EnvironmentSpec where

import           System.Environment.Compat
import           Test.Hspec

import           Test.Mockery.Environment

spec :: Spec
spec = do
  describe "withEnvironment" $ do
    it "runs the given action with the specified environment" $ do
      withEnvironment [("foo", "bar")] $ do
        getEnvironment `shouldReturn` [("foo", "bar")]

    it "restores the original environment" $ do
      old <- getEnvironment
      withEnvironment [("foo", "bar")] $ do
        return ()
      new <- getEnvironment
      new `shouldMatchList` old

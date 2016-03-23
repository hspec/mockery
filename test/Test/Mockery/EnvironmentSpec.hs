module Test.Mockery.EnvironmentSpec where

import           System.Environment.Compat
import           Test.Hspec

import           Test.Mockery.Environment

spec :: Spec
spec = describe "withEnvironment" $ do
  it "hides the environment" $ do
    withEnvironment [("foo", "bar")] $ do
      env <- getEnvironment
      env `shouldMatchList` [("foo", "bar")]
  it "restores the environment" $ do
    oldEnv <- getEnvironment
    withEnvironment [("foo", "bar")] $ do
      return ()
    newEnv <- getEnvironment
    newEnv `shouldMatchList` oldEnv
  it "should allow the environment to be modified" $ do
    withEnvironment [] $ do
      setEnv "foo2" "bar2"
      var <- lookupEnv "foo2"
      var `shouldBe` Just "bar2"

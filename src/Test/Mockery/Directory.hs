module Test.Mockery.Directory (
  inTempDirectory
, inTempDirectoryNamed
) where

import           Control.Exception
import           System.Directory
import           System.IO.Temp (withSystemTempDirectory)

inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "hspec" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action

inTempDirectoryNamed :: FilePath -> IO a -> IO a
inTempDirectoryNamed name action = inTempDirectory $ do
  createDirectory name
  setCurrentDirectory name
  action

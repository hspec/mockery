module Test.Mockery.Directory (
  inTempDirectory
, inTempDirectoryNamed
, withFile
) where

import           Control.Exception
import           System.Directory
import           System.IO hiding (withFile)
import           System.IO.Temp (withSystemTempDirectory, withSystemTempFile)

-- |
-- Run given action with the current working directory set to a temporary
-- directory.
--
-- The directory is created before the action is run.  After the action has
-- completed the directory is removed and the current working directory is
-- restored to its original value.
inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "mockery" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action

-- |
-- Similar to `inTempDirectory`, but the temporary directory will have the
-- specified name.
inTempDirectoryNamed :: FilePath -> IO a -> IO a
inTempDirectoryNamed name action = inTempDirectory $ do
  createDirectory name
  setCurrentDirectory name
  action

-- |
-- Create a temporary file with the given contents and execute the given
-- action.
--
-- The file is removed after the action has completed.
withFile :: String -> (FilePath -> IO a) -> IO a
withFile input action = withSystemTempFile "mockery" $ \file h -> do
  hPutStr h input
  hClose h
  action file

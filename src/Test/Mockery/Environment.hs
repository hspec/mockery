module Test.Mockery.Environment (withEnvironment) where

import           Control.Exception.Base
import           Control.Monad
import           System.Environment.Compat

-- |
-- Run the given action with the specified environment.
--
-- Before executing the action, `withEnvironment` backs up the current environment,
-- clears out the environment, and then applies the supplied environment.
-- After the action has completed the original environment is restored.
-- __Note__: The environment is global for a process, so tests that depend on the
-- environment can no longer be run in parallel.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment environment action = bracket saveEnv restoreEnv $ const action
  where
    saveEnv :: IO [(String, String)]
    saveEnv = do
      env <- clearEnv
      forM_ environment $ uncurry setEnv
      return env
    restoreEnv :: [(String, String)] -> IO ()
    restoreEnv env = do
      _ <- clearEnv
      forM_ env $ uncurry setEnv
    clearEnv :: IO [(String, String)]
    clearEnv = do
      env <- getEnvironment
      forM_ env (unsetEnv . fst)
      return env

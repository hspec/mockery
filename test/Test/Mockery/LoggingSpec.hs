module Test.Mockery.LoggingSpec (spec) where

import Test.Hspec
import Data.IORef
import System.Logging.Facade.Types
import System.Logging.Facade.Sink
import System.Logging.Facade as Log

import Test.Mockery.Logging

spec :: Spec
spec = describe "captureLogs" $ do
  let logToIORef :: IORef [LogRecord] -> LogSink
      logToIORef ref record = modifyIORef ref (record :)

  it "returns all log messages of an action" $ do
    (logs, ()) <- captureLogMessages $ do
      Log.trace "this should be captured"
      Log.trace "this should be captured next"
    logs `shouldBe` [ (TRACE, "this should be captured")
                    , (TRACE, "this should be captured next")
                    ]

  it "restores the original log sink" $ do
    ref <- newIORef []
    setLogSink $ logToIORef ref
    _ <- captureLogMessages $ Log.trace "this should be captured"
    Log.trace "this should not be captured"
    readIORef ref `shouldReturn` [LogRecord TRACE Nothing "this should not be captured"]

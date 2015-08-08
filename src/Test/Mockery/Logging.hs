{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Mockery.Logging (
  captureLogMessages
, captureLogMessages_
, LogLevel(..)
) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Exception
import           Data.IORef
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

-- | Capture all log messages produced by an IO action.
-- Logs are kept in memory.
captureLogMessages :: IO a -> IO ([(LogLevel, String)], a)
captureLogMessages action = bracket getLogSink setLogSink act
  where
    logToRef ref record = atomicModifyIORef' ref $ \logs -> (record : logs, ())
    unwrap LogRecord{..} = (logRecordLevel, logRecordMessage)
    act _  = do
      ref <- newIORef []
      setLogSink $ logToRef ref
      val <- action
      logs <- readIORef ref
      return (unwrap <$> reverse logs, val)

-- | Like 'captureLogsMessages', but ignores the result.
captureLogMessages_ :: IO a -> IO [(LogLevel, String)]
captureLogMessages_ action = fst <$> captureLogMessages action

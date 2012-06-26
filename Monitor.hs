module Monitor (
  monitor,
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time
import System.IO
import Text.Printf

monitor :: FilePath -> (String -> IO a) -> IO ()
monitor filename m = do
  r <- newIORef ""
  forever $ do
    cur <- readFile filename
    bef <- readIORef r
    length cur `seq` writeIORef r cur

    when (cur /= bef) $ try_ $ do
      _ <- m cur

      ct <- getCurrentTime
      tz <- getCurrentTimeZone
      printf "[%s]: update\n" (show $ utcToLocalTime tz ct)
      hFlush stdout

    threadDelay $ 10^(6::Int)

try_ :: IO a -> IO ()
try_ m = do
  ret <- try m
  case ret of
    Left (SomeException e) -> do
      print e
      hFlush stdout
    Right _ ->
      return ()

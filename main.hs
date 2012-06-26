import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.Time.LocalTime
import System.Environment
import System.IO
import Text.Pandoc
import Text.Printf

import HTML5Slide

main :: IO ()
main = do
  [filename] <- getArgs
  let outname = (++".html") $ takeWhile (/='.') filename
  r <- newIORef ""
  forever $ do
    try_  $ do
      strsrc <- readFile filename
      bef <- readIORef r
      when (bef /= strsrc) $ do
        let doc = readMarkdown defaultParserState { stateStandalone = True } strsrc
        writeFile outname $
          writeHTML5SlideString defaultWriterOptions doc
        writeIORef r strsrc
        cur <- getCurrentTime
        tz <- getCurrentTimeZone
        let lt = utcToLocalTime tz cur
        printf "[%s]: update\n" (show lt)
        hFlush stdout
    threadDelay $ 10^(6::Int)

try_ :: IO a -> IO ()
try_ m = do
  ret <- try m
  case ret of
    Left (SomeException e) -> do
      print e
    Right _ ->
      return ()

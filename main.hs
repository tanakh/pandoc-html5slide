import System.Console.CmdArgs
import Text.Pandoc

import HTML5Slide
import Monitor

main :: IO ()
main = do
  [filename] <- getArgs
  let outname = (++".html") $ takeWhile (/='.') filename
  monitor filename $ \strsrc -> do
    let doc = readMarkdown
              defaultParserState { stateStandalone = True }
              strsrc
    writeFile outname $
      writeHTML5SlideString defaultWriterOptions HTML5Slide.def doc

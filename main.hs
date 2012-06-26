{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

import Data.Maybe
import System.Console.CmdArgs as CA
import Text.Pandoc

import HTML5Slide as S
import Monitor

data HTML5Slide
  = HTML5Slide
    { argSlideScriptURL      :: Maybe String
    , argSlideClass     :: Maybe String
    , argSlideStyleCss  :: Maybe String
    , argSlideSyntaxCss :: Maybe String
    , filename :: String
    }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  HTML5Slide {..} <- cmdArgs $ HTML5Slide
    { argSlideScriptURL =
        CA.def
        &= explicit &= name "script-url"
        &= typ "URL"
        &= help "URL of Google HTML5 slide script"
    , argSlideClass =
        CA.def
        &= explicit &= name "slide-class"
        &= typ "CLASSNAME"
        &= help "CSS class name of slide"
    , argSlideStyleCss =
        CA.def
        &= explicit &= name "style-css"
        &= typFile
        &= help "Style CSS file"
    , argSlideSyntaxCss =
        CA.def
        &= explicit &= name "syntax-css"
        &= typFile
        &= help "Syntax CSS file"
    , filename =
      CA.def
      &= argPos 0 &= typFile
    }

  styleCss  <- maybe (return $ slideStyleCss  S.def) readFile argSlideStyleCss
  syntaxCss <- maybe (return $ slideSyntaxCss S.def) readFile argSlideSyntaxCss

  let outname = (++".html") $ takeWhile (/='.') filename
      slideOpt = S.def
        { slideScriptURL =
            fromMaybe (slideScriptURL S.def) argSlideScriptURL
        , slideClass =
            fromMaybe (slideClass S.def) argSlideClass
        , slideStyleCss =
            styleCss
        , slideSyntaxCss =
            syntaxCss
        }

  monitor filename $ \src -> do
    writeFile outname
      $ writeHTML5SlideString defaultWriterOptions slideOpt
      $ readMarkdown defaultParserState { stateStandalone = True } src

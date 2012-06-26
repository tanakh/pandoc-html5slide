{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module HTML5Slide (
  writeHTML5Slide,
  writeHTML5SlideString,
  ) where

import Control.Monad
import Data.Generics
import Data.List
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes as Attr
import Text.Hamlet
import Text.Pandoc
import Text.Pandoc.Highlighting

writeHTML5SlideString :: WriterOptions -> Pandoc -> String
writeHTML5SlideString opt pdoc = do
  renderHtml $ writeHTML5Slide opt pdoc

writeHTML5Slide :: WriterOptions -> Pandoc -> Html
writeHTML5Slide _ (Pandoc Meta {..} blocks) = [shamlet|
$doctype 5
<html>
  <head>
    <title>#{renderInlines $ sanitizeTitle docTitle}
    <meta charset="utf-8">
    <script src="http://html5slides.googlecode.com/svn/trunk/slides.js">
    <link rel="stylesheet" href="syntax.css">
    <link rel="stylesheet" href="style.css">

  <body style="display: none">
    <section.slides.layout-regular.template-pfi>
      <article>
        <h1>#{renderInlines docTitle}
        <p>
          $forall author <- docAuthors
            #{renderInlines author}
          <br>
          #{renderInlines docDate}

      $forall sec <- sectionize blocks
        <article>
          $forall s <- sec
            #{renderBlock s}
|]

sanitizeTitle :: [Inline] -> [Inline]
sanitizeTitle = everywhere (mkT $ replace "<br>" " ")

sectionize :: [Block] -> [[Block]]
sectionize [] = []
sectionize (b:bs) =
  let (cs, ds) = Prelude.span (not . isSplitter) bs
  in (b:cs) : sectionize ds

isSplitter (Header _ _) = True
isSplitter _ = False

sc :: String
sc = "\"sourceCode\""

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace from to ss
  | from `isPrefixOf` ss = to ++ drop (length from) ss
  | otherwise = Prelude.head ss : replace from to (tail ss)

{-
noprettify :: String -> String
noprettify "" = ""
noprettify ss
  | sc `isPrefixOf` ss =
    "\"noprettyprint " ++ tail (take (length sc) ss) ++
    noprettify (drop (length sc) ss)
  | otherwise =
      Prelude.head ss : noprettify (tail ss)
-}

renderBlock :: Block -> Html
renderBlock block = case block of
  Plain     inls -> mapM_ renderInline inls
  Para      inls -> p $ mapM_ renderInline inls

  CodeBlock attr codestr -> do
    case highlight formatHtmlInline attr codestr of
      Nothing -> error $ "code block error: " ++ codestr
      Just htm -> htm -- preEscapedToMarkup $ {-noprettify $-} XHtml.renderHtml htm

  RawBlock  format str -> preEscapedToMarkup str
  BlockQuote blocks -> blockquote $ mapM_ renderBlock blocks
  OrderedList attr bss -> do
    ol $ do
      forM_ bss $ \bs -> do
        li $ mapM_ renderBlock bs
  BulletList bss -> do
    ul $ do
      forM_ bss $ \bs -> do
        li $ mapM_ renderBlock bs
  DefinitionList defs -> do
    dl $ do
      forM_ defs $ \(t, ds) -> do
        dd $ mapM_ renderInline t
        forM_ ds $ \d ->
          mapM_ renderBlock d
  Header level inls -> do
    let h = case level of
          1 -> h1
          2 -> h2
          3 -> h3
          4 -> h4
          5 -> h5
          6 -> h6
          _ -> error ("header level: " ++ show level)
    h $ mapM_ renderInline inls
  HorizontalRule -> hr
  Table cap colAlign colWidthRatio colHeader rows -> do
    table $ do
      caption $ mapM_ renderInline cap
      thead $ tr $ do
        forM_ colHeader $ \co -> td $ do
          mapM_ renderBlock co
      tbody $ do
        forM_ rows $ \row -> tr $ do
          forM_ row $ \co -> td $ do
            mapM_ renderBlock co
  Null ->
    return ()

renderInlines :: [Inline] -> Html
renderInlines = mapM_ renderInline

renderInline :: Inline -> Html
renderInline inl = case inl of
  Str ss ->
    [shamlet|#{ss}|]
  Emph inls ->
    [shamlet|<em>#{renderInlines inls}|]
  Strong inls ->
    [shamlet|<strong>#{renderInlines inls}|]
  Strikeout inls ->
    [shamlet|<del>#{renderInlines inls}|]
  Superscript inls ->
    [shamlet|<sup>#{renderInlines inls}|]
  Subscript inls ->
    [shamlet|<sub>#{renderInlines inls}|]
  SmallCaps inls ->
    [shamlet|<small>#{renderInlines inls}|]
  Quoted SingleQuote inls ->
    [shamlet|'#{renderInlines inls}'|]
  Quoted DoubleQuote inls ->
    [shamlet|"#{renderInlines inls}"|]
  Cite cs inls ->
    -- TODO: use cite info
    [shamlet|<cite>#{renderInlines inls}|]
  Code ([], ["url"], []) code ->
    -- TODO: accept inline code
    [shamlet|#{code}|]
  Code attr code ->
    -- TODO: implement
    error $ show attr
  Space ->
    [shamlet|&nbsp;|]
  LineBreak ->
    [shamlet|<br>|]
  Math mathType str ->
    -- TODO: support it
    error "math"
  RawInline "html" str ->
    preEscapedToMarkup str
  RawInline format str ->
    -- TODO: support it
    error ("rawinline: " ++ format)
  Link inls (url, title) ->
    [shamlet|<a href="#{url}" alt="#{title}">#{renderInlines inls}|]
  Image inls (url, title) -> do
    [shamlet|<img.centered src="#{url}" alt="#{title}">|]
  Note _ ->
    -- TODO: support it
    error "note not supported"

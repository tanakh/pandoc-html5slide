{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module HTML5Slide (
  writeHTML5Slide,
  writeHTML5SlideString,
  ) where

import Data.Generics
import Data.List
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
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
        <article>#{renderBlocks sec}
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

renderBlocks :: [Block] -> Html
renderBlocks = mapM_ renderBlock

renderBlock :: Block -> Html
renderBlock block = case block of
  Plain     inls -> renderInlines inls
  Para      inls -> [shamlet|<p>#{renderInlines inls}|]

  CodeBlock attr codestr ->
    case highlight formatHtmlInline attr codestr of
      Nothing ->
        error $ "cannot highlighter for: " ++ show attr
      Just htm ->
        htm

  RawBlock  format str ->
    -- TODO: use format
    preEscapedToMarkup str
  BlockQuote blocks ->
    [shamlet|<blockquote>#{renderBlocks blocks}|]
  OrderedList attr bss ->
    [shamlet|
     <ol>
       $forall bs <- bss
         <li>#{renderBlocks bs}|]
  BulletList bss ->
    [shamlet|
     <ul>
       $forall bs <- bss
         <li>#{renderBlocks bs}|]
  DefinitionList defs ->
    [shamlet|
     <dl>
       $forall (t, ds) <- defs
         <dd>#{renderInlines t}
         $forall d <- ds
           #{renderBlocks d}|]
  Header level inls ->
    case level of
      1 -> [shamlet|<h1>#{renderInlines inls}|]
      2 -> [shamlet|<h2>#{renderInlines inls}|]
      3 -> [shamlet|<h3>#{renderInlines inls}|]
      4 -> [shamlet|<h4>#{renderInlines inls}|]
      5 -> [shamlet|<h5>#{renderInlines inls}|]
      6 -> [shamlet|<h6>#{renderInlines inls}|]
      _ -> error ("unsupported header level: " ++ show level)
  HorizontalRule ->
    [shamlet|hr|]
  Table cap colAlign colWidthRatio colHeader rows ->
    [shamlet|
     <table>
       <caption>#{renderInlines cap}
       <thead>
         <tr>
           $forall co <- colHeader
             <td>#{renderBlocks co}
       <tbody>
         $forall row <- rows
           <tr>
             $forall co <- row
               <td>#{renderBlocks co}|]
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module HTML5Slide (
  HTML5SlideOptions(..), def,
  writeHTML5Slide,
  writeHTML5SlideString,
  ) where

import Data.Default
import Data.Generics
import Data.List
import qualified Data.Text.Lazy as L
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Lucius
import Text.Pandoc
import Text.Pandoc.Highlighting

data HTML5SlideOptions
  = HTML5SlideOptions
    { slideClass     :: String
    , slideStyleCss  :: String
    , slideSyntaxCss :: String
    }

instance Default HTML5SlideOptions where
  def = HTML5SlideOptions
    { slideClass = "template-default"
    , slideStyleCss  = ""
    , slideSyntaxCss = L.unpack $ renderCss $ [lucius|
table.sourceCode,
tr.sourceCode,
td.lineNumbers,
td.sourceCode,
table.sourceCode pre {
  margin: 0;
  padding: 0;
  border: 0;
  vertical-align: baseline;
  border: none;
}
td.lineNumbers {
  border-right: 1px solid #AAAAAA;
  text-align: right;
  color: #AAAAAA;
  padding-right: 5px;
  padding-left: 5px;
}
td.sourceCode { padding-left: 5px; }
code.sourceCode span.kw { color: #007020; font-weight: bold; }
code.sourceCode span.dt { color: #902000; }
code.sourceCode span.dv { color: #40a070; }
code.sourceCode span.bn { color: #40a070; }
code.sourceCode span.fl { color: #40a070; }
code.sourceCode span.ch { color: #4070a0; }
code.sourceCode span.st { color: #4070a0; }
code.sourceCode span.co { color: #60a0b0; font-style: italic; }
code.sourceCode span.ot { color: #007020; }
code.sourceCode span.al { color: red; font-weight: bold; }
code.sourceCode span.fu { color: #06287e; }
code.sourceCode span.re { }
code.sourceCode span.er { color: red; font-weight: bold; }
|] undefined
    }

writeHTML5SlideString :: WriterOptions -> HTML5SlideOptions -> Pandoc -> String
writeHTML5SlideString opt sopt pdoc = do
  renderHtml $ writeHTML5Slide opt sopt pdoc

writeHTML5Slide :: WriterOptions -> HTML5SlideOptions -> Pandoc -> Html
writeHTML5Slide _ HTML5SlideOptions {..} (Pandoc Meta {..} blocks) = [shamlet|
$doctype 5
<html>
  <head>
    <title>#{renderInlines $ sanitizeTitle docTitle}
    <meta charset="utf-8">
    <script src="http://html5slides.googlecode.com/svn/trunk/slides.js">
    <style>#{slideStyleCss}
    <style>#{slideSyntaxCss}

  <body style="display: none">
    <section.slides.layout-regular.#{slideClass}>
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
  let (cs, ds) = span (not . isSplitter) bs
  in (b:cs) : sectionize ds
  where
    isSplitter (Header _ _) = True
    isSplitter _ = False

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace from to ss
  | from `isPrefixOf` ss = to ++ drop (length from) ss
  | otherwise = head ss : replace from to (tail ss)

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
      Just html ->
        html

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
      1 -> [shamlet|<h2>#{renderInlines inls}|]
      2 -> [shamlet|<h3>#{renderInlines inls}|]
      3 -> [shamlet|<h4>#{renderInlines inls}|]
      4 -> [shamlet|<h5>#{renderInlines inls}|]
      5 -> [shamlet|<h6>#{renderInlines inls}|]
      6 -> [shamlet|<h7>#{renderInlines inls}|]
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

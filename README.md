# google-html5-slide

This is a [Google HTML5 Slide](https://code.google.com/p/html5slides/) generator
for [Pandoc](http://johnmacfarlane.net/pandoc/) documents.

## Install

You can install from [Hackage](http://hackage.haskell.org/) using `cabal-install`.

~~~ {.bash}
$ cabal update
$ cabal install google-html5-slide
~~~

## Usage

Write a document supported by `Pandoc`
(currently, google-html-slide supports only markdown documents),
then convert it to HTML slide.

~~~ {.bash}
$ google-html5-slide your-slide.md
... (it generates your-slide.html) ...
~~~

Passing `--poll` option, you can poll filesystem for changes (like `omake -P`).

~~~ {.bash}
$ google-html5-slide your-slide.md --poll
[2012-06-26 20:02:25.2048234]: update
[2012-06-26 20:02:46.2600277]: update
[2012-06-26 20:02:48.3091449]: update
...
~~~

To know more detailed usage, please pass `--help` option.

## Example

You can find a translation of [Google HTML5 Demo Slide](http://html5slides.googlecode.com/svn/trunk/template/index.html) at [here](https://raw.github.com/tanakh/pandoc-html5slide/master/example/test.md).

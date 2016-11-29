{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module JS where

import Language.Javascript.JMacro
import Data.Text.Lazy as LT (Text)
import Text.PrettyPrint.Leijen.Text

data Transition = None | Cube | Page | Concave | Zoom | Linear 
-- default/cube/page/concave/zoom/linear/none


data RevealConfig  = RevealConfig {
    controls :: Bool
  , progress :: Bool
  , history :: Bool
  , center :: Bool
  , transition :: Transition
}
renderTransition :: Transition -> String
renderTransition None = "none"
renderTransition Cube = "cube"
renderTransition Page = "page"
renderTransition Concave = "concave"
renderTransition Zoom = "zoom"
renderTransition Linear = "linear"

defConfig :: RevealConfig
defConfig = RevealConfig True True True True None

revealInit :: RevealConfig -> LT.Text
revealInit conf =  displayT . renderOneLine . renderJs $ [jmacro|
        Reveal.initialize {
            keyboard: null

          , controls: `(controls conf)`
          , progress: `(progress conf)`
          , history: `(history conf)`
          , center: `(JS.center conf)`
          , transition: `(renderTransition $ transition conf)`

          , dependencies: `(dependencies)`
        };
|]
  where
    dependencies = [classlist, markdown, highlight]

    classlist = [jmacroE| {
      src: 'public/reveal.js-3.3.0/lib/js/classList.js',
      condition: function() { return !document.body.classList; }
    }|]

    markdown = [jmacroE| {
      src: 'public/reveal.js-3.3.0/plugin/markdown/marked.js',
      condition: function() { return !!document.querySelector( '[data-markdown]' ); }
    }|]

    highlight = [jmacroE| {
      src: 'public/highlight/highlight.pack.js',
      async: true,
      callback: function() { hljs.initHighlightingOnLoad(); }
    }|]



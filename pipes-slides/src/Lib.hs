{-# LANGUAGE OverloadedStrings #-}
module Lib (
    genIndex,
    Presentation(..), Topic(..), Slide,
    slide,
) where

import Lucid.Html5
import Lucid.Base
import Data.Monoid

import JS

import Data.Tree

-- | These correspond to what reveal.js is capable of displaying.
newtype Presentation = Presentation [Topic]
newtype Topic = Topic (Forest Slide)
newtype Slide = Slide { unSlide :: Html () }


renderPresentation :: Presentation -> Html ()
renderPresentation (Presentation topics) = page . reveal . slides . mconcat $
  section . mconcat . fmap subsection . renderTopic <$> topics
  where

    renderTopic :: Topic -> [Html ()]
    renderTopic (Topic slds) = concatMap renderSlideTree slds

    renderSlideTree :: Tree Slide -> [Html ()]
    renderSlideTree = fmap (mconcat . reverse . fmap unSlide) . pretrav []

    -- | Im not sure what to call this.  Not a traversal,
    --   just every possible path from root to every node.
    pretrav :: [a] -> Tree a -> [[a]]
    pretrav accum (Node c fs) = content : concatMap (pretrav content) fs
      where content = c : accum

genIndex :: Presentation -> IO ()
genIndex pres = renderToFile "index.html" (renderPresentation pres)


-- | These correlate to html elements required for reveal.js to function
reveal, slides, section, subsection :: Html () -> Html ()
reveal inner = with (div_ inner) [class_ "reveal"]
slides inner = with (div_ inner) [class_ "slides"]
section = section_ [class_ "sect"]
subsection = section_ [class_ "subsect"]

-- | Page head, js and css includes, etc.
page ::  Html () -> Html ()
page contents = doctypehtml_ (h <> b contents)
  where
    h :: Html ()
    b :: Html () -> Html ()
    h = head_ revealcss
    b contents' = body_ [style_ "background-color: black"]
      (contents' <> revealjs <> script_ [] (revealInit defConfig))

revealcss :: Html ()
revealcss =
  linkrel "public/highlight/styles/pojoaque.css" <>
  linkrel "public/reveal.js-3.3.0/css/reveal.css" <>
  linkrel "public/reveal.js-3.3.0/css/theme/black.css"
  where linkrel src = link_ [rel_ "stylesheet", href_ src]

revealjs :: Html ()
revealjs =
  with (script_ "") [src_ "public/reveal.js-3.3.0/lib/js/head.min.js"] <>
  with (script_ "") [src_ "public/reveal.js-3.3.0/js/reveal.js"]

slide :: Html () -> Forest Slide -> Tree Slide
slide = Node . Slide . div_

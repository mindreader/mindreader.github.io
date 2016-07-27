{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( genIndex
    ) where

import Lucid.Html5
import Lucid.Base
import Data.Monoid

import JS
import Snippets

newtype Slides = Slides [SlideSection]
newtype SlideSection = SlideSection [SlideSubSection]
data SlideSubSection =
  Fresh (Html ()) |
  ContinueLast (Html ()) |
  ContinueSecondToLast (Html ())


renderSlides :: Slides -> Html ()
renderSlides (Slides sections ) = page (rdiv $ rslide $ (mconcat $ map (rsect . renderSlideSection) sections :: Html ()))

renderSlideSection :: SlideSection -> Html ()
renderSlideSection (SlideSection ds) = fst $ foldl combine ("",[]) ds

combine :: (Html (), [Html ()]) -> SlideSubSection -> (Html(), [Html ()])
combine (h, _) (Fresh html) = (h <> rsect html, [html])
combine (h, ds) (ContinueLast html) = (h <> rsect (mconcat (reverse ds) <> html), html:ds)
combine (h, (_:d':ds)) (ContinueSecondToLast html) = (h <> rsect (mconcat (reverse (d':ds)) <> html), html:d':ds)
combine (h, _) (ContinueSecondToLast html) = (h <> html, [html]) -- might make more sense to just blow up.

genIndex :: IO ()
genIndex = renderToFile "index.html" (renderSlides presentation)
-- genIndex = renderToFile "index.html" (page presentation)


page ::  Html () -> Html ()
page contents = doctypehtml_ (h <> b contents)
  where
    h :: Html ()
    b :: Html () -> Html ()
    h = head_ revealcss
    b contents' = body_ [style_ "background-color: black"] (contents' <> revealjs <> script_ [] (revealInit defConfig))


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



presentation :: Slides
presentation =
  Slides [
    SlideSection [
      Fresh $ h1_ "Lens" <> with (h4_ "The practical use of Edward A. Kmett's lens library") [style_ "margin-bottom: 40px"],
      ContinueLast $ h5_ "View slides online at:" <> span_ "https://mindreader.github.io/lens-slides/index.html"
    ],

    SlideSection [
        Fresh $ span_ "It all starts with the optical" <> baseoptical
      , Fresh $ baseoptical <> opticalwitharrows
      , ContinueLast $ span_ "after some substitution" <> lenslike
      , Fresh $ span_ "everything has the same shape"
      , ContinueLast $ lens_types_1
      , ContinueSecondToLast $ lens_types_2
      , Fresh $ span_ "takes a function, returns a function" <> lenslikeasfunction
      , Fresh $ span_ "so you can string them together" <> type_1 <> type_2 <> type_just <> inst_choice_arrow
      , ContinueLast $ lensesstrungtogether
    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Fold" <> fold_type
      , Fresh $ fold_replicate
      , ContinueLast $ span_ "takes an a, returns many a's (or none)"
      , Fresh $ fold_replicate_mine
      , ContinueLast $ span_ "we have a fold. what do I do with it?"
      , Fresh $ fold_toList
      , Fresh $ fold_examples_1
      , Fresh $ foldable_class
      , Fresh $ fold_examples_1
      , Fresh $ fold_lenses
      , Fresh $ fold_examples_2
      , Fresh $ fold_worded_listed
      , Fresh $ fold_examples_3
      , Fresh $ span_ "and of course they can be combined" <> fold_examples_4
      , Fresh $ span_ "some other things you can do with Folds" <> fold_lenses_2
      , Fresh $ fold_examples_5
      , Fresh $ fold_examples_6
    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Getter" <> getter_type
      , Fresh $ span_ "a getter Getter a b is roughly the same as a function (a -> b)"
      , Fresh $ getter_operations
      , Fresh $ span_ "but weren't those functions Folds?" <> getter_next_to_fold
      , Fresh $ span_ "since a Getter is like a function, you can make one from one" <> to_types
      , Fresh $ getter_examples
      , Fresh $ span_ "mind bender: all getters are valid folds"
      , Fresh $ getter_as_folds_examples
    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Setter" <> setter_type
      , Fresh $ set_operations
      , Fresh $ ampersand_examples
      , Fresh $ setter_examples
      , Fresh $ setter_examples_2
      , Fresh $ setter_examples_3
      , Fresh $ setter_examples_5
      , Fresh $ span_ "there are specialized convenience setter operations" <> other_set_operations
      , Fresh $ setter_examples_4
    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Traversal" <> traversal_type
      , Fresh $ traverse_function
      , Fresh $ traverse_function_examples
      , Fresh $ each_type
      , Fresh $ each_fold_examples
      , Fresh $ each_setter_examples
      , Fresh $ each_examples
      , Fresh $ span_ "a Traversal can be made from anything that is already Traversable" <> traversable_class 
      , Fresh $ span_ "a Traversal can work on monomorphic types"
      , ContinueLast $ traversal_mono_examples
      , Fresh $ span_ "a Traversal is Applicative (and thus Monadic)"
      , ContinueLast $ traversal_monadic_examples
      , ContinueSecondToLast $ traversal_monadic_examples_2
    ],

    SlideSection [Fresh hierarchy],

    let ifyouhavelens = figure_ (figcaption_ "If you have a Lens' a b, you also have:")
    in SlideSection [
        Fresh $ h5_ "Lens" <> lens_type
      , Fresh $ ifyouhavelens
      , Fresh $ ifyouhavelens <> ul_ (li_ "a Getter a b")
      , Fresh $ ifyouhavelens <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b")
      , Fresh $ ifyouhavelens <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b" <> li_ "a Fold a b")
      , Fresh $ ifyouhavelens <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b" <> li_ "a Fold a b" <> li_ "a Traveral' a b")
      , Fresh $ span_ "a lens can be constructed from a get and modify function" <> lens_function
      , Fresh $ lens_function_examples
      , Fresh $ lens_function_examples_2
      , Fresh $ span_ "you can also derive Lens' from your own types via TH" <> derive_person_lens
      , Fresh $ span_ "resulting in these lenses" <> derive_person_lens_res
      , Fresh $ lens_function_examples_3
      , Fresh $ span_ "there isn't much you can't do at this point" <> lens_function_examples_4
    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Iso" <> iso_type
      , Fresh $ span_ "Iso comes from the word \"isomorphism\"" <> "which means roughly equivalent, the same, interchangeable"
      , Fresh $ span_ "you can construct an Iso from two functions (a -> b) and (b -> a)" <>  iso_function_examples
      , Fresh $ iso_function_examples_2
      , Fresh $ span_ "there are lots of useful isos" <> iso_function_examples_3
      , Fresh $ iso_function_examples_4

    ],

    SlideSection [Fresh hierarchy],

    SlideSection [
        Fresh $ h5_ "Prism" <> prism_type
      , Fresh $ span_ "A Prism a b allows for an a to possibly be converted to a b"
      , Fresh $ span_ "and if you managed to turn it into a b, then it definitely can be turned back into an a"
      , Fresh $ prism_function_examples
      , Fresh $ prism_function_examples_2
      , Fresh $ prism_function_examples_3
    ],

    SlideSection [
        Fresh $ h5_ "Plated (optional)" <> plated_type
      , Fresh $ plated_examples
      , Fresh $ plated_examples_2
    ],

    SlideSection [
        Fresh $ h5_ "Real life examples"
      , Fresh $ span_ "xml-html-conduit-lens" <> xml_html_conduit_lens_example
      , Fresh $ span_ "xml-html-conduit-lens" <> xml_html_conduit_lens_example_2
      , Fresh $ span_ "lens-aeson" <> lens_aeson_example 
      , Fresh $ span_ "lens-aeson" <> lens_aeson_example_2
    ],

    SlideSection [
        Fresh "Thank you!"
      , ContinueLast $ mconcat [
           h5_ "View slides online at:"
         , span_ "https://mindreader.github.io/lens-slides/index.html"
         , with (h5_ "Code for these slides:") [style_ "margin-top: 20px"]
         , span_ "https://github.com/mindreader/haskell-lens-slides"
         , with (h5_ "Questions, comments, corrections, clarifications:") [style_ "margin-top: 30px"]
         , span_ "david.mchealy@gmail.com"
        ]
    ]
  ]
  where
    hierarchy :: Html ()
    hierarchy = img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]

{-
presentation = rdiv $ rslide $
  rsect (h1_ "Lens" <> h4_ "The basic idea behind Edward A. Kmett's lens library") <>
  rsect (mconcat $ map rsect $ [
      span_ "It all starts with the optical" <> baseoptical
    , baseoptical <> opticalwitharrows
--    , h5_ "Perform some substitution with the arrows and you get"
    , baseoptical <> opticalwitharrows <> span_ "after some substitution" <> lenslike
    , span_ "everything has the same shape"
    , span_ "everything has the same shape" <> lens_types_1
    , span_ "everything has the same shape" <> lens_types_2
    , span_ "takes a function, returns a function" <> lenslikeasfunction
    , span_ "so you can string them together" <> type_1 <> type_2 <> type_just <> inst_choice_arrow
    , span_ "so you can string them together" <> type_1 <> type_2 <> type_just <> inst_choice_arrow <> lensesstrungtogether
    ]) <>

 rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Fold" <> fold_type
    , fold_replicate
    , fold_replicate <> span_ "takes an a, returns many a's (or none)"
    , fold_replicate_mine
    , fold_replicate_mine <> span_ "we have a fold. what do I do with it?"
    , fold_toList
    , fold_examples_1
    , foldable_class
    , fold_lenses
    , fold_examples_2
    , fold_worded_listed
    , fold_examples_3
    , span_ "and of course they can be combined" <> fold_examples_4
    , span_ "some other things you can do with Folds" <> fold_lenses_2
    , fold_examples_5
    , fold_examples_6
    ]) <>

  rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

  rsect (mconcat $ map rsect $ [
       h5_ "Getter" <> getter_type
     , span_ "a getter Getter a b is roughly the same as a function (a -> b)"
     , getter_operations
     , span_ "but weren't those functions Folds?" <> getter_next_to_fold
     , span_ "since a Getter is like a function, you can make one from one" <> to_types
     , getter_examples
     , span_ "mind bender: all getters are valid folds"
     , getter_as_folds_examples
    ]) <>

  rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

  rsect (mconcat $ map rsect $ [
       h5_ "Setter" <> setter_type
     , set_operations
--     , span_ "an aside: ATypes"
--     , span_ "an aside: ATypes" <> a_types
--     , span_ "an aside: ATypes" <> a_types <> span_ "purely about performance, completely replaceable"
     , ampersand_examples
     , setter_examples
     , setter_examples_2
     , setter_examples_3
     , setter_examples_5
     , span_ "there are specialized convenience setter operations" <> other_set_operations
     , setter_examples_4
    ]) <>
 
 rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Traversal" <> traversal_type
    , traverse_function
    , span_ "mindbender 2: all Traversals are Folds and Setters"
    , traverse_function_examples
    , each_type
    , each_fold_examples
    , each_setter_examples
    , each_examples
    , span_ "a Traversal can be made from anything that is already Traversable" <> traversable_class 
    , span_ "a Traversal can work on monomorphic types"
    , traversal_mono_examples
    , span_ "a Traversal is Applicative (and thus Monadic)"
    , traversal_monadic_examples
    , traversal_monadic_examples_2
    ]) <>

 rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

 rsect (mconcat $ map rsect $ [
       h5_ "Lens" <> lens_type
     , figure_ (figcaption_ "If you have a Lens' a b, you also have a ")
     , figure_ (figcaption_ "If you have a Lens' a b, you also have:" <> ul_ (li_ "a Getter a b"))
     , figure_ (figcaption_ "If you have a Lens' a b, you also have:" <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b"))
     , figure_ (figcaption_ "If you have a Lens' a b, you also have:" <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b" <> li_ "a Fold a b"))
     , figure_ (figcaption_ "If you have a Lens' a b, you also have:" <> ul_ (li_ "a Getter a b" <> li_ "a Setter' a b" <> li_ "a Fold a b" <> li_ "a Traveral' a b"))
     , span_ "a lens can be constructed from a get and modify function" <> lens_function
     , lens_function_examples
     , lens_function_examples_2
     , span_ "you can also derive Lens' from your own types via TH" <> derive_person_lens
     , span_ "resulting in these lenses" <> derive_person_lens_res
     , lens_function_examples_3
     , span_ "there isn't much you can't do at this point" <> lens_function_examples_4
    ]) <>
  
 rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Iso" <> iso_type
    , span_ "Iso comes from the word \"isomorphism\"" <> "which means roughly equivalent, the same, interchangeable"
    , span_ "you can construct an Iso from two functions (a -> b) and (b -> a)" <>  iso_function_examples
    , span_ "there are lots of useful isos" <> iso_function_examples_2
    , iso_function_examples_3
    , iso_function_examples_4
    ]) <>

 rsect (mconcat $ map rsect $ [ img_ [style_ "width: 50%; height: 100%", src_ "https://imgur.com/ALlbPRa.png"]  ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Prism" <> prism_type
    , span_ "A Prism a b allows for an a to possibly be converted to a b"
    , span_ "and if you managed to turn it into a b, then it definitely can be turned back into an a"
    , prism_function_examples
    , prism_function_examples_2
    , prism_function_examples_3
    ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Plated (optional)" <> plated_type
    , plated_examples
    , plated_examples_2
    ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Real life examples"
    , span_ "xml-html-conduit-lens" <> xml_html_conduit_lens_example
    , span_ "xml-html-conduit-lens" <> xml_html_conduit_lens_example_2
    , span_ "lens-aeson" <> lens_aeson_example 
    , span_ "lens-aeson" <> lens_aeson_example_2
    ]) <>

 rsect (mconcat $ map rsect $ [
      h5_ "Thank you!"
    ])
-}

{-
  rsect (mconcat $ map rsect $ [
        span_ "manual derivation"
      , span_ "manual derivation" <> country_snippet_1
      , span_ "manual derivation" <> country_snippet_1 <> country_snippet_2
      , span_ "in practice that is never necessary"
      , derive_person_lens
      , derive_person_lens <> derive_person_lens_res

--      , span_ "you can also derive prisms from algebraic types"
--      , derive_gender_prisms
--      , derive_gender_prisms <> derive_gender_prisms_res
    ])
-}

rdiv, rslide, rsect :: Html () -> Html ()
rdiv inner = with (div_ inner) [class_ "reveal"]
rslide inner = with (div_ inner) [class_ "slides"]
rsect = section_

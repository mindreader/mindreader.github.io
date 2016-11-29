{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Snippets where

import Data.String.QQ

import Lucid.Base (Html, toHtmlRaw)
-- import Lucid.Html5 (pre_, code_, class_)

import Language.Haskell.HsColour.HTML
import Language.Haskell.HsColour.Colourise

subduedred = Rgb 182 73 38
lighttan = Rgb 220 207 143
slightorange = Rgb 181 137 0
darkblue = Rgb 88 110 117
slightlylighterbg = Rgb 24 25 20

myColourPrefs = ColourPrefs
  { keyword  = [Foreground subduedred] -- let
  , keyglyph = [Foreground subduedred] -- like =

  , layout   = [Foreground lighttan] -- parentheses, brackets, commas
  , comment  = [Foreground darkblue, Italic]
  , conid    = [Foreground slightorange, Normal] -- class constraints
  , varid    = [Foreground lighttan, Normal] -- function names
  , conop    = [Foreground lighttan] -- like 2:[5], the :
  , varop    = [Foreground lighttan]

  , string   = [Foreground darkblue]
  , char     = [Foreground darkblue]
  , number   = [Foreground darkblue]

  , cpp      = [Foreground lighttan,Dim]
  , selection = [Foreground lighttan]
  , variantselection = [Dim, Foreground Red, Underscore]
  , definition = [Foreground lighttan, Normal]
  }
{-
testsnippet = hcode [s|
  let x = ['a', 'b', 'c']
  let x = 2:[5]
  let
    myfold :: Fold a a
    myfold = replicated 5

 type Getter s a =
  (Contravariant f, Functor f) =>
    (a -> f a) -> s -> f s

  toListOf :: Getting (Data.Monoid.Endo [a]) s a -> s -> [a]
  (^..) :: s -> Getting (Data.Monoid.Endo [a]) s a -> [a]

  -- idealized
  toListOf :: Fold a b -> a -> [b]
  (^..) :: a -> Fold a b -> [b]

  to :: (Profunctor p, Contravariant f) =>
              (s -> a) -> Optic' p f s a
  to show :: (Show a, Profunctor p, Contravariant f) =>
              Optic' p f a String

  > view (to (+1)) 2
  > 2 ^. to (+1)
  3

  > view (to (+1) . to show) 1
  > 1 ^. to (+1) . to show
  "2"

  > toListOf (to show . replicated 2) (1,2)
  > (1,2) ^.. to show . replicated 2
  ["(1,2)","(1,2)"]
|]
-}

hcode :: String -> Html ()
hcode = toHtmlRaw . hscolour myColourPrefs False 0

proxydeclaration = hcode [s|
   Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
|]

proxytype = hcode [s|
  data Proxy a' a b' b m r
|]

producertype = hcode [s|
  type Producer b = Proxy X () () b 
|]

consumertype = hcode [s|
  type Consumer a = Proxy () a () X
|]

pipetype = hcode [s|
  type Pipe a b = Proxy () a () b 
|]

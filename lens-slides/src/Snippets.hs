{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Snippets where

import Data.Monoid

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



hcode' :: Html () -> Html ()
hcode' inner = pre_ (with (code_ inner) [class_ "hs"])

-}
hcode :: String -> Html ()
hcode = toHtmlRaw . hscolour myColourPrefs False 0


baseoptical = hcode [s|
  type Optical p q f s t a b = p a (f b) -> q s (f t)
|]

opticalwitharrows = hcode [s|
  type LensLike = Optical (->) (->) f s t a b
|]

lenslike = hcode [s|
type LensLike = (a -> f b) -> s -> f t
|]

lenslikeasfunction = hcode [s|
                 (a -> f b) -> (s -> f t)
|]

lensesstrungtogether = hcode [s|
_1 . _Just . _2  ::
        (Applicative f, Field2 a b a1 b1,
         Field1 s t (Maybe a) (Maybe b)) =>
             (a1 -> f b1) -> s -> f t)
|]

type_1 = hcode "_1 :: (Functor f, Field1 s t a b) => (a -> f b) -> s -> f t"
type_2 = hcode "_2 :: (Functor f, Field2 s t a b) => (a -> f b) -> s -> f t"

inst_choice_arrow = hcode [s|
     instance Choice (->)
|]

type_just = hcode [s|
_Just :: (Applicative f, Choice p) =>
              p a (f b) -> p (Maybe a) (f (Maybe b))
|]

country_snippet_1 = hcode [s|

  data Country = Country {
    cname :: Name,
    cpopulation :: Integer
  } deriving Show
  |]

country_snippet_2 = hcode [s|
  -- cname_lens :: Lens' Country Name
  cname_lens :: Functor f =>
     (Name -> f Name) -> Country -> f Country
  cname_lens f oldcountry = fmap
       (\newname -> oldcountry { cname = newname })
       (f $ cname oldcountry)

  |]

getter_operations = hcode [s|
  view :: Control.Monad.Reader.Class.MonadReader s m =>
             Getting a s a -> m a
  (^.) :: s -> Getting a s a -> a

  -- idealized
  view :: Getter a b -> a -> b
  (^.) :: a -> Getter a b -> b
 |]

getter_next_to_fold = hcode [s|
  view :: Getter a b -> a -> b
  (^.) :: a -> Getter a b -> b

  view :: Monoid b => Fold a b -> a -> b
  (^.) :: Monoid b => a -> Fold a b -> b
|]

set_operations = hcode [s|
  set :: ASetter s t a b -> b -> s -> t
  (.~) :: ASetter s t a b -> b -> s -> t

  -- idealized
  set :: Setter' a b -> b -> a -> a
  (.~) :: Setter' a b -> b -> a -> a

  over :: ASetter s t a b -> (a -> b) -> s -> t 
  (%~) :: ASetter s t a b -> (a -> b) -> s -> t

  -- idealized
  over :: Setter' a b -> (b -> b) -> a -> a
  (%~) :: Setter' a b -> (b -> b) -> a -> a

  -- utility function
  (&) :: a -> (a -> b) -> b -- ($) flipped
|]

other_set_operations = hcode [s|
  -- +, -, *, /
  (+~) :: Num a => ASetter s t a a -> a -> s -> t
  (-~) :: Num a => ASetter s t a a -> a -> s -> t
  (*~) :: Num a => ASetter s t a a -> a -> s -> t
  (//~) :: Fractional a => ASetter s t a a -> a -> s -> t
  -- raise to a power
  (^~) :: (Num a, Integral e) => ASetter s t a a -> e -> s -> t 
  -- logical or, logical and
  (||~) :: ASetter s t Bool Bool -> Bool -> s -> t
  (&&~) :: ASetter s t Bool Bool -> Bool -> s -> t
  -- monoidal appending
  (<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
|]

getter_examples = hcode [s|
  > view (to show) 1
  > 1 ^. to show
  > "1"

  > view (to (+1)) 2
  > 2 ^. to (+1)
  3

  > view (to (+1) . to show) 1
  > 1 ^. to (+1) . to show
  "2"
|]

getter_as_folds_examples = hcode [s|
  > sumOf (to (+1)) 1
  2

  > toListOf (to show) 1
  > 1 ^.. to show
  "1"

  -- folds and getters can be mixed, become folds (idealized)
  to show . replicated 2 :: (Show a) => Fold a String

  > toListOf (to show . replicated 2) (1,2)
  > (1,2) ^.. to show . replicated 2
  ["(1,2)","(1,2)"]
|]


ampersand_examples = hcode [s|
  > 1 & print
  > print $ 1
  "1"

  >[1,2,3] & map show
  >map show $ [1,2,3]
  ["1","2","3"]
|]

setter_examples = hcode [s|
  > set id 2 1
  > 2 & id .~ 1
  1

  > set id [1,2,3] 1
  > [1,2,3] & id .~ 1
  1

  > over id (+1) 1
  > 1 & over id (+1)
  2

  > 1 & over id (+1) . over id (+2)
  4
|]  

setter_examples_2 = hcode [s|
  sets :: ((a -> b) -> s -> t) -> Setter s t a b
  -- idealized
  sets :: ((a -> a) -> b -> b) -> Setter' b a

  let
    mysetfunc :: (b -> b) -> (a,b,c) -> (a,b,c)
    mysetfunc f (a,b,c) = (a, f b, c)

    mysetter :: Setter' (a,b,c) b
    mysetter = sets mysetfunc
|]

setter_examples_3 = hcode [s|
  > set mysetter 4 (1,2,3)
  > (1,2,3) & mysetter .~ 4
  (1,4,3)
 
  > over mysetter (+21) (1,2,3)
  > (1,2,3) & mysetter %~ (+21)
  (1,23,3)
|]

setter_examples_4 = hcode [s|
  -- addition
  > 2 & id +~ 2
  4

  -- boolean logic
  > True & id &&~ False
  False

  -- monoidal concattenation
  > [1,2,3] & id <>~ [4,5,6] & id <>~ [7,8]
  [1,2,3,4,5,6,7,8]
|]

setter_examples_5 = hcode [s|
  mapped :: (Functor f, Settable f1) =>
    (a -> f1 b) -> f a -> f1 (f b)

  -- idealized
  mapped :: Functor f => Setter' (f a) a

  > over mapped show [1,2,3]
  ["1","2","3"]

  > over (mapped . mapped) show [[1,2],[3,4]]
  [["1","2"],["3","4"]]

  > over (mapped . mapped . mapped) (+1)
          [[Just 1,Just 2],[Nothing,Just 4]]
  [[Just 2,Just 3],[Nothing,Just 5]]
|]




to_types = hcode [s|
  to :: (Profunctor p, Contravariant f) =>
              (s -> a) -> Optic' p f s a
  to show :: (Show a, Profunctor p, Contravariant f) =>
              Optic' p f a String

  -- idealized
  to :: (a -> b) -> Getter a b

  show :: (Show a) -> a -> b
  (+1) :: (Num a) -> a -> a

  to show :: (Show a) => Getter a b
  to (+1) :: (Num a) => Getter a a
|]

setter_type = hcode [s|
  type Setter s t a b =
    Settable f =>
      (a -> f b) -> s -> f t

  type Setter' s a = Simple Setter = Setter s s a a
 |]

getter_type = hcode [s|
  type Getter s a =
    (Contravariant f, Functor f) =>
      (a -> f a) -> s -> f s
 |]

lens_type = hcode [s|
  type Lens s t a b =
      Functor f => (a -> f b) -> s -> f t

  type Lens' s a = Simple Lens = Lens s s a a
|]

fold_type = hcode [s|
  type Fold s a =
      (Contravariant f, Applicative f) =>
        (a -> f a) -> s -> f s
|]

fold_replicate = hcode [s|
  replicated :: (Applicative f, Contravariant f) =>
     Int -> (a -> f a) -> a -> f a

  -- idealized
  replicated :: Int -> Fold a a

|]

fold_replicate_mine = hcode [s|
  let
    myfold :: Fold a a
    myfold = replicated 5
|]

fold_toList = hcode [s|
  -- You can turn it into a list
  toListOf :: Getting (Data.Monoid.Endo [a]) s a -> s -> [a]
  (^..) :: s -> Getting (Data.Monoid.Endo [a]) s a -> [a]

  -- idealized
  toListOf :: Fold a b -> a -> [b]
  (^..) :: a -> Fold a b -> [b]
|]



iso_type = hcode [s|
  type Iso s t a b =
    (Profunctor p, Functor f) =>
      p a (f b) -> p s (f t)

  type Iso' s a = Simple Iso = Iso s s a a
|]

prism_type = hcode [s|
  type Prism s t a b =
    (Choice p, Applicative f) =>
      p a (f b) -> p s (f t)

  type Prism' s a = Simple Prism = Prism s s a a
|]

traversal_type = hcode [s|
  type Traversal s t a b =
    Applicative f => (a -> f b) -> s -> f t

  type Traversal' s a = Simple Traversal = Traversal s s a a
|]

plated_type = hcode [s|
  class Plated a where
    plate :: Traversal' a a

  instance Plated [a]
  instance Plated (Tree a)

  data Person = Person Name Gender [Person] deriving Show
  parents :: Lens' Person [Person]

  instance Plated Person where
    plate = parents . traverse
|]

plated_examples = hcode [s|
  l ... m = l . plate . m

  > putin ^.. pname
  [Name "Vladimir Vladimirovich Putin"]

  > putin ^.. id ... pname -- children
  [Name "Maria Ivanovna Putina",
   Name "Vladimir Spiridonovich Putin"]

  > putin ^.. id ... id ... pname -- grand children
  []
|]

plated_examples_2 = hcode [s|
  > let newputin =
     putin & over (parents . element 0 . parents)
        (<> [Person "mchealy" Male []])

  > newputin ^.. id ... id ... pname
  [Name "mchealy"]
|]

xml_html_conduit_lens_example = hcode [s|

  xml :: HaxXML a => Traversal' a Element
  instance HasXML Document
  instance HasXML LT.Text
  instance Plated Element

  import qualified Data.Text.Lazy as LT
  let doc :: LT.Text
      doc = "<root><channel><title>craigslist louisville
              </title><channel></root>"

  > :t  doc ^. xml
  [Element {elementName = Name {nameLocalName = "root", name ...
|]
xml_html_conduit_lens_example_2 = hcode [s|
  >doc ^. xml . name
  "root"

  > doc ^. xml ... name . text
  "channel"

  > doc ^. xml ... texts
  "craigslist louisville"

  > let
      channeln :: Traversal' Element Element
      channeln = node "{http://purl.org/rss/1.0/}channel"

  -- assuming doc has namespaces
  > doc ^.. xml . channeln . name :: [Text]
|]

lens_aeson_example = hcode [s|
  -- create aeson values
  > _Integer # 1 :: Value
  Number 1.0
  
  :set -XOverloadedLists, :set XOverloadedStrings
  > _Object # [("key", _String # "value")] :: Text
  "{\"key\":\"value\"}"


  > _Object # [("key", _String # "value")] :: Value
  Object (fromList [("key",String "value")])

  -- piece of cake
|]
lens_aeson_example_2 = hcode [s|
  -- parse json
  > "[1,2,3,4,\"adsf\"]" ^? _Array
  Just [Number 1.0,Number 2.0,Number 3.0,Number 4.0,String "adsf"]

  > "{\"key\":\"value\"}" ^? _Array
  Nothing

  > "[1,2,3,4,\"adsf\"]" ^? _Value
  Just (Array [
   Number 1.0,Number 2.0,Number 3.0,Number 4.0,String "adsf"
  ])

  -- piece of cake
|]

each_type = hcode [s|
  class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
    each :: Traversal s t a b

  -- idealized
  each :: Each a a b b => Traversal' a b

  instance Each [a] [b] a b
  instance Each (a,b) (c,d) a c
  instance Each (a,b,c) (d,e,f) a d
  instance Each (Maybe a) (Maybe b) a b
  ...
|]

each_examples = hcode [s|
  -- looked at differently
  each :: (Each a..., Applicative f) => (a -> f a) -> a -> f a

  > each print [1,2] :: IO [()]
  1
  2

  > each print ('a','b') :: IO [()]
  'a'
  'b'

  > (each . each) (putStrLn . show) (Just [2]) :: IO (Maybe [()])
  2

  > each (putStrLn . show) (Nothing :: Maybe Int) :: IO (Maybe ())
  (prints nothing)
|]

each_fold_examples = hcode [s|
  > toListOf each [1,2,3]
  > [1,2,3] ^.. each
  [1,2,3]

  > toListOf each (1,2,3,4)
  > (1,2,3,4) ^.. each
  [1,2,3,4]

  > sumOf each (1,2,3)
  6

  > nullOf each []
  True
|]

each_setter_examples = hcode [s|
  > set each 4 [1,2,3]
  > [1,2,3] & set each 4
  [4,4,4]

  > over (each . each . each) negate (Just ([1],[2,3]))
  > (Just ([1],[2,3])) & over (each . each . each) negate
  Just ([-1],[-2,-3])

  -- warning, each and traverse are not always interchangeable
  > over (each . traverse . each) negate (Just ([1],[2,3]))
  > (Just ([1],[2,3])) & over (each . traverse . each) negate
  Just ([1],[-2,-3])
|]

traverse_function = hcode [s|
  traverse :: (Applicative f, Traversable t) =>
    (a -> f b) -> t a -> f (t b)

  -- idealized
  traverse :: (Traversable t) => Traversal' (t a) (t b)

  instance Traversable []
  instance Traversable Maybe
  instance Traversable (Either a)
  instance Traversable Tree
  -- others
|]

traverse_function_examples = hcode [s|
  > toListOf traverse [1,2,3]
  > [1,2,3] ^.. traverse
  [1,2,3]

  > sumOf traverse [1,2,3]
  6

  > over (traverse . traverse) (+1) [Left 'a',Right 2]
  > [Left 'a', Right 2] & over (traverse . traverse) (+1)
  > [Left 'a', Right 3]

  > over (traverse . traverse . traverse)
         negate (Just [[1,2],[3,4]])
  > (Just [[1,2],[3,4]]) & over
          (traverse . traverse . traverse) negate
  Just ([-1,-2],[-3,-4])
|]

lens_function = hcode [s|
  lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
  lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
|]

lens_function_examples = hcode [s|
  data Person = Person Name Gender [Person] deriving Show
  newtype Name = Name T.Text deriving Show

  putin, hismom, hisdad :: Person
  putin = Person "Vladimir Vladimirovich Putin"
                 Male [hismom, hisdad]
  hismom = Person ...
  hisdad = Person ...

  let mylens :: Lens' Person Name
      mylens = lens
        (\(Person n _ _) -> n)
        (\(Person old g p) new -> Person new g p)
|]

lens_function_examples_2 = hcode [s|
  mylens :: Lens' Person Name

  > putin
  Person (Name "Vladimir Vladimirovich Putin") Male [Per...

  > view mylens putin
  > putin ^. mylens
  Name "Vladimir Vladimirovich Putin"

  > set mylens (Name "mchealy") putin
  > putin & mylens .~ Name "mchealy"
  Person (Name "mchealy") Male [Person "Maria "...
|]

lens_function_examples_3 = hcode [s|
  > view pname putin
  > putin ^. pname
  Name "Vladimir Vladimirovich Putin"

  > view parents putin
  > putin ^. parents
  [Person (Name "Maria Ivanovna Putina") Female [],
   Person (Name "Vladimir Spiridonovich Putin") Male []]

  > toListOf (parents . traverse . pname) putin
  > putin ^.. parents . traverse . pname
  [Name "Maria Ivanovna Putina",
   Name "Vladimir Spiridonovich Putin"]
|]

lens_function_examples_4 = hcode [s|
  > putin ^? parents . traversed .
             filtered (not . isFemale) . pname
  Just (Name "Vladimir Spiridonovich Putin")

  > let flipgender Male = Female; flipgender Female = Male
  > putin & over (parents . traversed . gender) flipgender
  Person (Name "Vladimir Vladimirovich Putin") Male [
    Person (Name "Maria Ivanovna Putina") Male [],
    Person (Name "Vladimir Spiridonovich Putin") Female []
  ]
|]

iso_function_examples = hcode [s|
  iso :: (a -> b) -> (b -> a) -> Iso' a b

  from :: (Functor f, Profunctor p) =>
     AnIso s t a b -> p t (f s) -> p b (f a)
  -- idealized
  from :: Iso' a b -> Iso' b a
|]

iso_function_examples_2 = hcode [s|
  import Data.Char
  let myiso :: Iso' Char Char
      myiso = iso toUpper toLower

  > view myiso 'a'
  > 'a' ^. myiso
  -- also 'A' ^. myiso
  'A'

  > view (from myiso) 'A'
  > 'A' ^. from myiso
  -- also 'a' ^. from myiso
  'a'
|]

iso_function_examples_3 = hcode [s|
  enum :: Enum a => Iso' Int a
  reversed :: Reversing a => Iso' a a 
  instance Reversing Text
  instance Reversing String
  .. others

  lazy :: (Functor f, Profunctor p, Strict lazy strict) =>
     p lazy (f lazy) -> p strict (f strict)
  -- idealized
  lazy :: Strict a b => Iso' b a
  instance Strict Data.Text.Lazy Data.Text
  instance Strict Data.ByteString.Lazy Data.ByteString
|]

iso_function_examples_4 = hcode [s|
  >"asdf" ^. reversed ^. from reversed
  "asdf"
  
  >:t ("asdf" ::  T.Text) ^. lazy
  ("asdf" ::  T.Text) ^. lazy :: LT.Text

  > ("asdf" ::  T.Text) & lazy .~ ("asdf" :: LT.Text)
  "asdf" -- :: LT.Text
  >("asdf" ::  LT.Text) & from lazy .~ ("asdf" :: T.Text)
  "asdf" -- :: T.Text

  >97 ^. enum :: Char
 'a'

  ("asdf" :: [Char]) ^.. traverse . from enum
  [97,115,100,102]
|]


prism_function_examples = hcode [s|
  re :: Contravariant f => AReview t b -> LensLike' f b t
  -- idealized
  re :: Prism' a b -> Getter b a

  -- allows s and t to differ
  prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b 

  -- more often used
  prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b 

  -- for our uses
  prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
|]

prism_function_examples_2 = hcode [s|
  import Data.Int

  let myprism :: Prism' Int64 Int8
      myprism = prism' fromIntegral (\i ->
        if i <= 127 && i >= (-128)
          then Just (fromIntegral i)
          else Nothing)

  > 12 ^? myprism
  Just 12

  > 999 ^? myprism
  Nothing

  >:t  12 ^. re myprism
  12 ^. re myprism :: Int64
|]

prism_function_examples_3 = hcode [s|
  Data.Text.Lazy.utf8 :: Prism' ByteString Text
  
  -- This bs could have invalid characters in it
  > ("asdf" :: ByteString) ^? utf8
  Just "asdf" -- :: Text

  -- Text can always be converted back to bytestring
  > ("asdf" :: Text) ^. re utf8
  "asdf" -- :: ByteString
|]



wrapped_example = hcode [s|
  data Person = Person Name Gender [Person] deriving Show
  newtype Name = Name T.Text deriving (Show)
  makeWrapped ''Name -- I'll get to this later

  mylens :: Lens' Person Name
  mylens = lens
    (\(Person n _ _) -> n)
    (\(Person old g p) new -> Person new g p)

  > putin ^. mylens -- view mylens putin
  Name "Vladimir Vladimirovich Putin"

  > putin & pname . _Wrapped %~ T.toLower
  Person (Name "vladimir vladimirovich putin") Male [Pers ...
|]

lens_types_1 = hcode [s|
type Lens s t a b =
  Functor f =>
    (a -> f b) -> s -> f t

type Setter s t a b =
  Settable f =>
    (a -> f b) -> s -> f t

type Getter s a =
  (Contravariant f, Functor f) =>
    (a -> f a) -> s -> f s

type Iso s t a b =
  (Profunctor p, Functor f) =>
    (a -> f b) -> s -> f t
  |]

lens_types_2 = hcode [s|

type Traversal s t a b =
  Applicative f => (a -> f b) -> s -> f t

type Fold s a =
  (Contravariant f, Applicative f) =>
    (a -> f a) -> s -> f s

type Prism s t a b =
  (Choice p, Applicative f) =>
    p a (f b) -> p s (f t)
 |]


derive_person_lens = hcode ([s|
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

...

declareLenses [d|

  data Person = Person {
    pname :: Name,
    pgender :: Gender,
    parents :: [Person]
  } deriving Show

  |] <> "|]")

derive_person_lens_res = hcode [s|
  data Person = Person Name Gender [Person]

  pname :: Lens' Person Name
  pgender :: Lens' Person Gender
  parents :: Lens' Person [Person]

|]

derive_gender_prisms = hcode ([s|

declarePrisms [d|

  data Gender = Male | Female
  
  data IP = IP
  data Connection = Disconnected | Connected IP

  |] <> "|]")

derive_gender_prisms_res = hcode [s|

  _Male :: Prism' Gender ()
  _Female :: Prism' Gender ()

  _Disconnected :: Prism' Connection ()
  _Connected :: Prism' Connection IP

  |]




foldable_class = hcode [s|
  class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
    toList :: t a -> [a]
    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
    maximum :: forall a . Ord a => t a -> a
    minimum :: forall a . Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a
 |]

traversable_class = hcode [s|
  class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    sequence :: Monad m => t (m a) -> m (t a) 

  instance Traversable []
  instance Traversable Maybe
  instance Traversable (Either a)
  -- amongst many others

  -- note that traverse from Traversable is already a usable lens
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse :: Traversal' (t a) (t b)
 |]

fold_lenses = hcode [s|
  foldOf :: Getting a s a -> s -> a
  foldMapOf :: Getting r s a -> (a -> r) -> s -> r
  foldrOf :: Getting (Data.Monoid.Endo r) s a ->
                (a -> r -> r) -> r -> s -> r
  toListOf :: Getting (Data.Monoid.Endo [a]) s a -> s -> [a]

  nullOf :: Getting Data.Monoid.All s a -> s -> Bool
  lengthOf ::
    Getting (Data.Monoid.Endo (Data.Monoid.Endo Int)) s a ->
       s -> Int
  -- all the others...
|]


traversal_mono_examples = hcode [s|
  -- idealized
  Data.Text.Lens.text :: IsText t => Traversal' t Char
  instance IsText [Char] -- aka String
  instance IsText Text
  instance IsText ByteString
  
  > ("abc" :: [Char]) & over text succ
  > ("abc" :: Text) & over text succ
  "bcd"
|]

traversal_monadic_examples = hcode [s|
  Data.Text.Lens.text :: IsText t => Traversal' t Char
  -- idealized
  Data.Text.Lens.text :: (Applicative f, IsText t) =>
     (Char -> f Char) -> t -> f t

  > text print "12" -- try to print each character
  Couldn't match type '()' with 'Char'

  print :: Show a => a -> IO () -- <- that () is the problem
|]
traversal_monadic_examples_2 = hcode [s|
  let myprint :: Show a => a -> IO a
      myprint a = do
        print a
        return a

  text myprint "12" :: IO String
  '1'
  '2'
  (returns "12")

  -- (>=>) is like the (.) of Monads
  text (myprint >=> myprint) "1" :: IO String
  '1'
  '1'
  (returns "1")
|]

fold_examples_1 = hcode [s|
  > toListOf mylens 2
  [2,2,2,2,2]

  > 3 ^.. mylens
  [3,3,3,3,3]
|]
fold_examples_2 = hcode [s|
  > lengthOf mylens 1
  5

  > sumOf mylens 2
  10

  > nullOf mylens 1
  False

  > nullOf (replicated 0) 1
  True
|]

fold_worded_listed = hcode [s|
  worded :: (Applicative f, Indexable Int p) =>
     p String (f String) -> String -> f String
  lined :: (Applicative f, Indexable Int p) =>
     p String (f String) -> String -> f String

  -- idealized
  worded :: Fold String String
  lined :: Fold String String
|]

fold_examples_3 = hcode [s|
  > toListOf worded "ipso facto"
  > "ipso facto" ^.. worded
  ["ipso","facto"]

  > toListOf lined "asdf\nqwerty asdf\n"
  > "asdf\nqwerty asdf\n" ^.. lined
  ["asdf", "qwerty asdf"]
|]

fold_examples_4 = hcode [s|
  > let txt = "asdf fdsa\nfoo bar baz\nyo"

  > toListOf (lined . worded) txt
  > txt ^.. lined . worded
  ["asdf","fdsa","foo","bar","baz","yo"]

  > toListOf (lined . replicated 2 . worded) txt
  > txt ^.. lined . replicated 2 . worded
  ["asdf","fdsa","asdf","fdsa","foo","bar","baz",
   "foo","bar","baz","yo","yo"]

  > txt ^.. replicated 2 . lined . worded
  ["asdf","fdsa","foo","bar","baz","yo",
   "asdf","fdsa","foo","bar","baz","yo"]
|]


fold_lenses_2 = hcode [s|
  firstOf :: Getting (Leftmost a) s a -> s -> Maybe a
  (^?) :: s -> Getting (First a) s a -> Maybe a

  -- idealized
  firstOf :: Fold a b -> a -> Maybe b
  (^?) :: a -> Fold a b -> Maybe b

  view :: Control.Monad.Reader.Class.MonadReader s m =>
          Getting a s a -> m a
  (^.) :: (^.) :: s -> Getting a s a -> a

  --idealized
  view :: Monoid b => Fold a b -> a -> b
  (^.) :: Monoid b => a -> Fold a b -> b
|]

fold_examples_5 = hcode [s|
  > firstOf (replicated 3) 1
  > 1 ^? replicated 3
  Just 1

  > firstOf (replicated 0) 1
  > 1 ^? replicated 0
  Nothing
|]

fold_examples_6 = hcode [s|
  -- the b in Fold a b must be a monoid!
  > view worded "asdf fdsa"
  > "asdf fdsa" ^. worded
  "asdffdsa"

  -- Nums are not monoids (they cannot be concattenated)
  > view (replicated 2) 1
  No instance for (Num a0) arising from a use of ...

  -- Num a => Sum a is monoidal, it just adds successive numbers
  import Data.Monoid
  > view (replicated 2) (Sum 1)
  > Sum 1 ^. replicated 2
  Sum { getSum = 2 }
|]

{-
  folded :: Foldable f => IndexedFold Int (f a) a 
  filtered :: (Choice p, Applicative f) =>
    (a -> Bool) -> Optic' p f a a 
  worded :: Applicative f =>
    IndexedLensLike' Int f String String

fold_examples_4 = hcode [s|
  > putin ^.. parents . traverse . filtered isFemale 
  [Person (Name "Maria Ivanovna Putina") Female []]

  > putin ^. parents . traverse . filtered isFemale 
  No instance for (Monoid Person) arising from a use...

  > putin ^. parents . traverse . filtered isFemale .
               to (\x -> [x])
  [Person (Name "Maria Ivanovna Putina") Female []]

  > putin ^? parents . traverse . filtered isFemale
  Just (Person (Name "Maria Ivanovna Putina") Female [])

  > putin ^. parents & has (ix 1)
  True
|]

fold_examples_5 = hcode [s|
  > [(1,2),(3,4)] ^.. folded . both
  > [1,2,3,4]

  > [(1,2),(3,5),(4,4)] ^.. folded . folded . filtered even
  [2,4]

|]
-}

{-
more_view_examples = hcode [s|
  > Map.fromList [("asdf",1),("fdsa",2)] ^. at "fdsa"
  Just 2
  > [(Just (1,2)),Nothing] ^? element 0 . _Just . _2
  Just 1
  > Just Nothing ^? _Just . _Just
  Nothing
|]
-}

{-
view_reader_example = hcode [s|
  readerNames :: Person -> [Name]
  readerNames = runReader familynames
    where
      familynames :: Reader Person [Name]
      familynames = do
        cname <- view pname
        pnames <- do
          parents <- view parents
          return $ parents ^.. traverse . pname
        return (cname : pnames)

  >readerNames putin
  [Name "Vladimir Vladimirovich Putin",
   Name "Maria Ivanovna Putina",
   Name "Vladimir Spiridonovich Putin"]
 |]
-}
{-
traversal_examples = hcode [s|
  > toListOf traverse [1,2,3]
  > [1,2,3] ^.. traverse
  [1,2,3]

  > over traverse show [1,2,3]
  > [1,2,3] & over traverse show
  ["1","2","3"]
  
  > toListOf (traverse . traverse) [[1,2],[3,4]]
  > [[1,2],[3,4]] ^.. traverse . traverse
  [1,2,3,4]

  > over (traverse . traverse) (+1) [Just 1,Nothing, Just 2]
  > [Just 1,Nothing, Just 2] & over (traverse . traverse) (+1)
  [Just 2,Nothing,Just 3]
|]
-}

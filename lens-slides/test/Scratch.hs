{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Scratch where

import Control.Lens

import Data.String
import qualified Data.Text as T

newtype Name = Name T.Text deriving (Show, IsString)
makeWrapped ''Name



data IP

declarePrisms [d|
  data Gender = Male | Female deriving Show
  data Connection = Disconnected | Connected IP
 |]


declareLenses [d|

  data Person = Person {
    pname :: Name,
    pgender :: Gender,
    parents :: [Person]
  } deriving Show

 |]

instance Plated Person where
  plate = parents . traverse

data Country = Country {
  _cname :: Name,
  _cpopulation :: Integer
} deriving Show

makeLenses ''Country


myname :: Lens' Country Name
-- myname :: Functor f => (Name -> f Name) -> Country -> f Country
myname f oldcountry = fmap
  (\newname -> oldcountry { _cname = newname })
  (f $ _cname oldcountry)

putin = Person "Vladimir Vladimirovich Putin" Male [hismom, hisdad]
hismom = Person "Maria Ivanovna Putina" Female []
hisdad = Person "Vladimir Spiridonovich Putin" Male []

putinsnamelowered :: Name
putinsnamelowered = putin ^. pname & _Wrapped %~ T.toLower
lowerputinsname :: Person
lowerputinsname = putin & pname . _Wrapped %~ T.toLower

flipgender Male = Female
flipgender Female = Male

isFemale (Person _ Female _) = True
isFemale _ = False


parentsgender :: [Gender]
parentsgender = putin ^.. parents . traverse . pgender

makeparentsmale :: [Person]
makeparentsmale = putin ^. parents & traverse . pgender .~ Male

flipparentgenders :: Person
flipparentgenders = putin & parents . traverse . pgender %~ flipgender

putinsdad :: Maybe Person
putinsdad = putin ^? parents . traversed . filtered (not . isFemale)


-- putin ^.. takingWhile (isFemale) (parents . folded)
-- putin ^.. pname . _Wrapped . _Text . unpacked . worded
-- putin ^.. parents . traverse . filtered isFemale
--
-- Just ((1,2),(3,4)) & over (_Just . both . _2) (+1) (also each)
-- [1,2,3] & anyOf each even
-- ("asdf" :: T.Text) & traverseOf_ Data.Text.Lens.text print
-- [(1,2)] & over (element 1 . both) (+1)
-- >toListOf (traverse . _1) [(1,2),(2,3),(3,4)]
-- [1,2,3]
-- >toListOf (traverse . _2) [(1,2),(2,3),(3,4)]
-- [2,3,4]

--
-- # :: AReview t b -> b -> t (alias for review)
--
-- > 128 ^? integral :: Maybe Int8
-- Nothing
-- > 125 ^? integral :: Maybe Int8
-- Just 125
-- > (125 :: Int8) ^. re integral
-- 125
--
-- > (1, 2, 3, 4, 5) ^.. (_2 <> _3 <> _5)
-- [2,3,5]
--
-- (1, 2, 3, 4, 5) ^.. (_2 <> _3 <> _5)
--
-- (1,2,3) ^.  _1
-- (1,2,3) ^..  _1
-- (1,2,3) ^..  _1 <> _2
--
-- folds... check
-- getters... check
-- setters... check
-- traversals... check
-- isos
-- [optional] wrapped
-- prisms
-- [optional] plated
--
-- let mom = putin ^. parents ^.. filtered isFemale
-- view for getters / lenses
-- set for setters / lenses
-- preview / review for prism
-- from turns an iso around

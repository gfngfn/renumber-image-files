module Types where

import qualified Data.Map.Strict as Map

-- The type for tags (that represent authors)
type Tag = String

-- The type for 1-origin numbers for (sets of) works
type Number = Int

-- The type for 1-origin indices within a set of works sharing the same number
type Index = Int

-- The type for filename extensions (e.g. ".jpg", ".png")
type Extension = String

newtype FileInfo = FileInfo (Tag, Number, Maybe Index, Extension)
  deriving Eq

instance Ord FileInfo where
  compare finfo1 finfo2 =
    let
      FileInfo (tag1, n1, iopt1, ext1) = finfo1
      FileInfo (tag2, n2, iopt2, ext2) = finfo2
    in
    case (tag1, n1) `compare` (tag2, n2) of
      LT -> LT
      GT -> GT
      EQ ->
        case (iopt1, iopt2) of
          (Nothing, Nothing) -> ext1 `compare` ext2
          (Just _, Nothing)  -> GT
          (Nothing, Just _)  -> LT
          (Just i1, Just i2) -> (i1, ext1) `compare` (i2, ext2)


data Error
  = CannotParseFileName   String
  | SingleAlreadyExists   Tag Number Extension Index Extension
  | MultipleAlreadyExists Tag Number (Map.Map Index Extension) Extension
  | DuplicatedSingle      Tag Number Extension Extension
  | DuplicatedMultiple    Tag Number Index Extension Extension

module Types where

import qualified Data.Map.Strict as Map


type Tag = String

type Number = Int

type Index = Int

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
  = SingleAlreadyExists   Tag Number Extension Index Extension
  | MultipleAlreadyExists Tag Number (Map.Map Index Extension) Extension
  | DuplicatedSingle      Tag Number Extension Extension
  | DuplicatedMultiple    Tag Number Index Extension Extension

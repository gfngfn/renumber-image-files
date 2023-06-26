module Types where

import Data.Map.Strict (Map)
import Data.Set (Set)

-- The type for tags (that represent authors)
type Tag = String

-- The type for 1-based numbers for (sets of) works
type Number = Int

-- The type for 1-based indices within a set of works sharing the same number
type Index = Int

-- Type type for texts for classification
type Class = String

-- The type for filename extensions (e.g. "jpg", "png")
type Extension = String

type Property = (Set Class, Extension)

newtype FileInfo = FileInfo (Tag, Number, Maybe Index, Property)
  deriving (Eq, Ord)

data Error
  = CannotParseFileName   String
  | SingleAlreadyExists   Tag Number Property Index Property
  | MultipleAlreadyExists Tag Number (Map Index Property) Property
  | DuplicatedSingle      Tag Number Property Property
  | DuplicatedMultiple    Tag Number Index Property Property

newtype RenumberInfo = RenumberInfo (FileInfo, Number)

type NextNumberMap = Map Tag Number

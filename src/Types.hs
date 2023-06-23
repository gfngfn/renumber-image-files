module Types where

import qualified Data.Map.Strict as Map

-- The type for tags (that represent authors)
type Tag = String

-- The type for 1-based numbers for (sets of) works
type Number = Int

-- The type for 1-based indices within a set of works sharing the same number
type Index = Int

-- Type type for texts for classification
type Class = String

-- The type for filename extensions (e.g. ".jpg", ".png")
type Extension = String

newtype FileInfo = FileInfo (Tag, Number, Maybe Index, [Class], Extension)
  deriving (Eq, Ord)

data Error
  = CannotParseFileName   String
  | SingleAlreadyExists   Tag Number ([Class], Extension) Index ([Class], Extension)
  | MultipleAlreadyExists Tag Number (Map.Map Index ([Class], Extension)) ([Class], Extension)
  | DuplicatedSingle      Tag Number ([Class], Extension) ([Class], Extension)
  | DuplicatedMultiple    Tag Number Index ([Class], Extension) ([Class], Extension)


newtype RenumberInfo = RenumberInfo (FileInfo, Number)

type NextNumberMap = Map.Map Tag Number

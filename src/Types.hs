module Types where

import qualified Data.Map.Strict as Map


type Tag = String

type Number = Int

type Index = Int

type Extension = String

type FileInfo = (Tag, Number, Maybe Index, Extension)

data Error
  = SingleAlreadyExists   Tag Number Extension Index Extension
  | MultipleAlreadyExists Tag Number (Map.Map Index Extension) Extension
  | DuplicatedSingle      Tag Number Extension Extension
  | DuplicatedMultiple    Tag Number Index Extension Extension

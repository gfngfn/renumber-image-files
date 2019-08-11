module Types where

import qualified Data.Map.Strict as Map

type Tag = String

data Kind
  = Single Int
  | Multiple Int Int
  deriving Show

type Extension = String

type FileInfo = (Tag, Int, Maybe Int, Extension)

type TagMap = Map.Map Tag ()

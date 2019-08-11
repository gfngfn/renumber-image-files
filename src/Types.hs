module Types where


data Kind
  = Single Int
  | Multiple Int Int
  deriving Show


type Extension = String

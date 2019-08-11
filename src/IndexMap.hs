module IndexMap where

import qualified Data.Map.Strict as Map

import Types


type IndexMap = (Tag, Number, IndexMapMain)

data IndexMapMain
  = Empty
  | Single Extension
  | Multiple MultipleMap

type MultipleMap = Map.Map Index Extension


empty :: Tag -> Number -> IndexMap
empty tag n =
  (tag, n, Empty)


add :: Maybe Index -> Extension -> IndexMap -> Either Error IndexMap
add iopt ext (tag, n, main) =
  case (main, iopt) of
    (Empty, Nothing)            -> Right (tag, n, Single ext)
    (Empty, Just i)             -> Right (tag, n, Multiple (Map.singleton i ext))
    (Single ext1, Just i)       -> Left (SingleAlreadyExists tag n ext1 i ext)
    (Single ext1, Nothing)      -> Left (DuplicatedSingle tag n ext1 ext)
    (Multiple multMap, Nothing) -> Left (MultipleAlreadyExists tag n multMap ext)
    (Multiple multMap, Just i)  -> addToMultipleMap tag n i ext multMap


addToMultipleMap :: Tag -> Number -> Index -> Extension -> MultipleMap -> Either Error IndexMap
addToMultipleMap tag n i ext multMap =
  case Map.lookup i multMap of
    Nothing   -> Right (tag, n, Multiple (Map.insert i ext multMap))
    Just ext1 -> Left (DuplicatedMultiple tag n i ext1 ext)

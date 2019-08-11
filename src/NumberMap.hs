module NumberMap where

import qualified Data.Map.Strict as Map

import Types
import qualified IndexMap


type NumberMap = (Tag, Map.Map Number IndexMap.IndexMap)


empty :: Tag -> NumberMap
empty tag =
  (tag, Map.empty)


add :: Number -> Maybe Index -> Extension -> NumberMap -> Either Error NumberMap
add n iopt ext (tag, mapMain) =
  case Map.lookup n mapMain of
    Nothing -> do
      indexMap <- IndexMap.add iopt ext (IndexMap.empty tag n)
      Right (tag, Map.insert n indexMap mapMain)

    Just indexMap -> do
      indexMapNew <- IndexMap.add iopt ext indexMap
      Right (tag, Map.insert n indexMapNew mapMain)

module TagMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Types
import NumberMap (NumberMap)
import NumberMap qualified

type TagMap = Map Tag NumberMap

empty :: TagMap
empty = Map.empty

add :: Tag -> Number -> Maybe Index -> Property -> TagMap -> Either Error TagMap
add tag n iopt prop tagMap =
  case Map.lookup tag tagMap of
    Nothing -> do
      numberMap <- NumberMap.add n iopt prop (NumberMap.empty tag)
      return (Map.insert tag numberMap tagMap)
    Just numberMap -> do
      numberMapNew <- NumberMap.add n iopt prop numberMap
      return (Map.insert tag numberMapNew tagMap)

getRenumberInfos :: NextNumberMap -> TagMap -> ([RenumberInfo], NextNumberMap)
getRenumberInfos preMap tagMap =
  let
    (infoss, numNextMap) =
      Map.foldlWithKey
        (\(acc, mapacc) tag numMap ->
           let numPrev = Maybe.fromMaybe 1 (Map.lookup tag preMap) in
           let (infos, numNext) = NumberMap.getRenumberInfos numPrev numMap in
           (infos : acc, Map.insert tag numNext mapacc))
        ([], Map.empty) tagMap
  in
  (List.reverse (List.concat infoss), numNextMap)

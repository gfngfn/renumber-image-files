module TagMap where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Types
import qualified NumberMap


type TagMap = Map.Map Tag NumberMap.NumberMap

type NextNumberMap = Map.Map Tag Number


empty :: TagMap
empty = Map.empty


add :: Tag -> Number -> Maybe Index -> Extension -> TagMap -> Either Error TagMap
add tag n iopt ext tagMap =
  case Map.lookup tag tagMap of
    Nothing -> do
      numberMap <- NumberMap.add n iopt ext (NumberMap.empty tag)
      return (Map.insert tag numberMap tagMap)

    Just numberMap -> do
      numberMapNew <- NumberMap.add n iopt ext numberMap
      return (Map.insert tag numberMapNew tagMap)


getRenumberInfos :: TagMap -> ([RenumberInfo], NextNumberMap)
getRenumberInfos tagMap =
  let
    (infoss, numNextMap) =
      Map.foldlWithKey
        (\(acc, mapacc) tag numMap ->
           let (infos, numNext) = NumberMap.getRenumberInfos numMap in
           (infos : acc, Map.insert tag numNext mapacc))
        ([], Map.empty) tagMap
  in
  (List.concat infoss, numNextMap)

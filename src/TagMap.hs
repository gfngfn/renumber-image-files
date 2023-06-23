module TagMap where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Types
import qualified NumberMap


type TagMap = Map.Map Tag NumberMap.NumberMap


empty :: TagMap
empty = Map.empty


add :: Tag -> Number -> Maybe Index -> ([Class], Extension) -> TagMap -> Either Error TagMap
add tag n iopt classAndExt tagMap =
  case Map.lookup tag tagMap of
    Nothing -> do
      numberMap <- NumberMap.add n iopt classAndExt (NumberMap.empty tag)
      return (Map.insert tag numberMap tagMap)

    Just numberMap -> do
      numberMapNew <- NumberMap.add n iopt classAndExt numberMap
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

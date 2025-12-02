module NumberMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List qualified as List

import Types
import IndexMap (IndexMap)
import IndexMap qualified

type NumberMap = (Tag, Map Number IndexMap)

empty :: Tag -> NumberMap
empty tag =
  (tag, Map.empty)

add :: Number -> Maybe Index -> Property -> NumberMap -> Either Error NumberMap
add n iopt classAndExt (tag, mapMain) =
  case Map.lookup n mapMain of
    Nothing -> do
      let indexMap = IndexMap.singleton tag n iopt classAndExt
      Right (tag, Map.insert n indexMap mapMain)
    Just indexMap -> do
      indexMapNew <- IndexMap.add iopt classAndExt indexMap
      Right (tag, Map.insert n indexMapNew mapMain)

getRenumberInfos :: Number -> NumberMap -> ([RenumberInfo], Number)
getRenumberInfos numPrev (_tag, mapMain) =
  let
    (numNext, renumberInfos) =
      Map.foldl
        (\(numNew, acc) indexMap ->
           let infos = IndexMap.getRenumberInfos indexMap numNew in
           ((numNew :: Int) + 1, infos : acc))
        (numPrev, []) mapMain
  in
  (List.concat renumberInfos, numNext)

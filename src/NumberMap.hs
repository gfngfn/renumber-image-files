module NumberMap where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Types
import qualified IndexMap


type NumberMap = (Tag, Map.Map Number IndexMap.IndexMap)


empty :: Tag -> NumberMap
empty tag =
  (tag, Map.empty)


add :: Number -> Maybe Index -> ([Class], Extension) -> NumberMap -> Either Error NumberMap
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

module IndexMap where

import qualified Data.Map.Strict as Map

import Types


type IndexMap = (Tag, Number, IndexMapMain)

data IndexMapMain
  = Single ([Class], Extension)
  | Multiple MultipleMap

type MultipleMap = Map.Map Index ([Class], Extension)


singleton :: Tag -> Number -> Maybe Index -> ([Class], Extension) -> IndexMap
singleton tag n iopt classAndExt =
  case iopt of
    Nothing -> (tag, n, Single classAndExt)
    Just i  -> (tag, n, Multiple (Map.singleton i classAndExt))


add :: Maybe Index -> ([Class], Extension) -> IndexMap -> Either Error IndexMap
add iopt classAndExt (tag, n, main) =
  case (main, iopt) of
    (Single classAndExt1, Just i)  -> Left (SingleAlreadyExists tag n classAndExt1 i classAndExt)
    (Single classAndExt1, Nothing) -> Left (DuplicatedSingle tag n classAndExt1 classAndExt)
    (Multiple multMap, Nothing) -> Left (MultipleAlreadyExists tag n multMap classAndExt)
    (Multiple multMap, Just i)  -> addToMultipleMap tag n i classAndExt multMap


addToMultipleMap :: Tag -> Number -> Index -> ([Class], Extension) -> MultipleMap -> Either Error IndexMap
addToMultipleMap tag n i classAndExt multMap =
  case Map.lookup i multMap of
    Nothing           -> Right (tag, n, Multiple (Map.insert i classAndExt multMap))
    Just classAndExt1 -> Left (DuplicatedMultiple tag n i classAndExt1 classAndExt)


getRenumberInfos :: IndexMap -> Number -> [RenumberInfo]
getRenumberInfos (tag, numOld, main) numNew =
  case main of
    Single (classes, ext) ->
      [RenumberInfo (FileInfo (tag, numOld, Nothing, classes, ext), numNew)]

    Multiple multMap ->
      Map.foldlWithKey
        (\acc i (classes, ext) -> RenumberInfo (FileInfo (tag, numOld, Just i, classes, ext), numNew) : acc)
        [] multMap

module IndexMap where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Types

type IndexMap = (Tag, Number, IndexMapMain)

data IndexMapMain
  = Single Property
  | Multiple MultipleMap

type MultipleMap = Map Index Property

singleton :: Tag -> Number -> Maybe Index -> Property -> IndexMap
singleton tag n iopt classAndExt =
  case iopt of
    Nothing -> (tag, n, Single classAndExt)
    Just i  -> (tag, n, Multiple (Map.singleton i classAndExt))

add :: Maybe Index -> Property -> IndexMap -> Either Error IndexMap
add iopt classAndExt (tag, n, main) =
  case (main, iopt) of
    (Single classAndExt1, Just i)  -> Left (SingleAlreadyExists tag n classAndExt1 i classAndExt)
    (Single classAndExt1, Nothing) -> Left (DuplicatedSingle tag n classAndExt1 classAndExt)
    (Multiple multMap, Nothing) -> Left (MultipleAlreadyExists tag n multMap classAndExt)
    (Multiple multMap, Just i)  -> addToMultipleMap tag n i classAndExt multMap

addToMultipleMap :: Tag -> Number -> Index -> Property -> MultipleMap -> Either Error IndexMap
addToMultipleMap tag n i classAndExt multMap =
  case Map.lookup i multMap of
    Nothing           -> Right (tag, n, Multiple (Map.insert i classAndExt multMap))
    Just classAndExt1 -> Left (DuplicatedMultiple tag n i classAndExt1 classAndExt)

getRenumberInfos :: IndexMap -> Number -> [RenumberInfo]
getRenumberInfos (tag, numOld, main) numNew =
  case main of
    Single prop ->
      [RenumberInfo (FileInfo (tag, numOld, Nothing, prop), numNew)]
    Multiple multMap ->
      Map.foldlWithKey
        (\acc i prop -> RenumberInfo (FileInfo (tag, numOld, Just i, prop), numNew) : acc)
        [] multMap

module IndexMap where

import qualified Data.Map.Strict as Map

import Types


type IndexMap = (Tag, Number, IndexMapMain)

data IndexMapMain
  = Single Extension
  | Multiple MultipleMap

type MultipleMap = Map.Map Index Extension


singleton :: Tag -> Number -> Maybe Index -> Extension -> IndexMap
singleton tag n iopt ext =
  case iopt of
    Nothing -> (tag, n, Single ext)
    Just i  -> (tag, n, Multiple (Map.singleton i ext))


add :: Maybe Index -> Extension -> IndexMap -> Either Error IndexMap
add iopt ext (tag, n, main) =
  case (main, iopt) of
    (Single ext1, Just i)       -> Left (SingleAlreadyExists tag n ext1 i ext)
    (Single ext1, Nothing)      -> Left (DuplicatedSingle tag n ext1 ext)
    (Multiple multMap, Nothing) -> Left (MultipleAlreadyExists tag n multMap ext)
    (Multiple multMap, Just i)  -> addToMultipleMap tag n i ext multMap


addToMultipleMap :: Tag -> Number -> Index -> Extension -> MultipleMap -> Either Error IndexMap
addToMultipleMap tag n i ext multMap =
  case Map.lookup i multMap of
    Nothing   -> Right (tag, n, Multiple (Map.insert i ext multMap))
    Just ext1 -> Left (DuplicatedMultiple tag n i ext1 ext)


getRenumberInfos :: IndexMap -> Number -> [RenumberInfo]
getRenumberInfos (tag, numOld, main) numNew =
  case main of
    Single ext ->
      [RenumberInfo (FileInfo (tag, numOld, Nothing, ext), numNew)]

    Multiple multMap ->
      Map.foldlWithKey
        (\acc i ext -> RenumberInfo (FileInfo (tag, numOld, Just i, ext), numNew) : acc)
        [] multMap

module Lib where

import qualified Data.Ord
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.Set as Set

import Types
import qualified FileNameParser
import qualified TagMap


checkFileList :: Maybe (Set.Set String) -> [String] -> ([Error], Either [Error] TagMap.TagMap)
checkFileList tagRestriction fnames = do
  let (errsParse, finfos) = Either.partitionEithers (List.map parseFileName fnames)
  let
    isNeededTag :: String -> Bool
    isNeededTag =
      case tagRestriction of
        Nothing     -> const True
        Just tagSet -> (`Set.member` tagSet)

    (errsDup, tagMap) =
      makeValidationMap isNeededTag (List.sortOn Data.Ord.Down finfos)

  case errsDup of
    []    -> (errsParse, Right tagMap)
    _ : _ -> (errsParse, Left errsDup)


parseFileName :: String -> Either Error FileInfo
parseFileName fname =
  case FileNameParser.parse fname of
    Just finfo -> Right finfo
    Nothing    -> Left (CannotParseFileName fname)


makeValidationMap :: (String -> Bool) -> [FileInfo] -> ([Error], TagMap.TagMap)
makeValidationMap isNeededTag files =
  let
    validateSingle :: ([Error], TagMap.TagMap) -> FileInfo -> ([Error], TagMap.TagMap)
    validateSingle (errAcc, tagMap) finfo =
      let FileInfo (tag, n, iopt, ext) = finfo in
      if isNeededTag tag then
        case TagMap.add tag n iopt ext tagMap of
          Right tagMapNew -> (errAcc, tagMapNew)
          Left err        -> (err : errAcc, tagMap)
      else
        (errAcc, tagMap)
  in
  List.foldl validateSingle ([], TagMap.empty) files

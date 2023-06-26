module Lib where

import Data.Ord qualified as Ord
import Data.List qualified as List
import Data.Either qualified as Either
import Data.Set (Set)
import Data.Set qualified as Set

import Types
import FileNameParser qualified
import TagMap (TagMap)
import TagMap qualified

checkFileList :: Maybe (Set Tag) -> [FilePath] -> ([Error], Either [Error] TagMap)
checkFileList tagRestriction fnames = do
  let (errsParse, finfos) = Either.partitionEithers (List.map parseFileName fnames)
  let
    isNeededTag :: String -> Bool
    isNeededTag =
      case tagRestriction of
        Nothing     -> const True
        Just tagSet -> (`Set.member` tagSet)
    (errsDup, tagMap) =
      makeValidationMap isNeededTag (List.sortOn Ord.Down finfos)
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
      let FileInfo (tag, n, iopt, prop) = finfo in
      if isNeededTag tag then
        case TagMap.add tag n iopt prop tagMap of
          Right tagMapNew -> (errAcc, tagMapNew)
          Left err        -> (err : errAcc, tagMap)
      else
        (errAcc, tagMap)
  in
  List.foldl validateSingle ([], TagMap.empty) files

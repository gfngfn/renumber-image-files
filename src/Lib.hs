module Lib where

import qualified Data.Ord
import qualified Data.List as List
import qualified Data.Either as Either

import Types
import qualified FileNameParser
import qualified TagMap


checkFileList :: [String] -> ([Error], Either [Error] TagMap.TagMap)
checkFileList fnames = do
  let (errsParse, finfos) = Either.partitionEithers (List.map parseFileName fnames)
  let (errsDup, tagMap) = makeValidationMap (List.sortOn Data.Ord.Down finfos)
  case errsDup of
    []    -> (errsParse, Right tagMap)
    _ : _ -> (errsParse, Left errsDup)


parseFileName :: String -> Either Error FileInfo
parseFileName fname =
  case FileNameParser.parse fname of
    Just finfo -> Right finfo
    Nothing    -> Left (CannotParseFileName fname)


makeValidationMap :: [FileInfo] -> ([Error], TagMap.TagMap)
makeValidationMap files =
  let
    validateSingle :: ([Error], TagMap.TagMap) -> FileInfo -> ([Error], TagMap.TagMap)
    validateSingle (errAcc, tagMap) finfo =
      let FileInfo (tag, n, iopt, ext) = finfo in
      case TagMap.add tag n iopt ext tagMap of
        Right tagMapNew -> (errAcc, tagMapNew)
        Left err        -> (err : errAcc, tagMap)
  in
  List.foldl validateSingle ([], TagMap.empty) files

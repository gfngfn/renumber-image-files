module Lib where

import qualified Data.Ord
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import Control.Monad

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


printError :: Error -> IO ()
printError err =
  case err of
    CannotParseFileName fname ->
      putStrLn $ "! Cannot parse '" ++ fname ++ "'"

    SingleAlreadyExists tag n ext1 i ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n (Just i) ext2
      in
      mapM_ putStrLn
        [ "! There already exists a single:"
        , "  * " ++ fname1
        , "  and thus cannot validate:"
        , "  * " ++ fname2
        ]

    MultipleAlreadyExists tag n multMap ext2 ->
      let
        fname2 = showFile tag n Nothing ext2
      in do
        putStrLn $ "! There already exists a multiple:"
        printMultiple tag n multMap
        putStrLn $ "  and should rename:"
        putStrLn $ "  * " ++ fname2

    DuplicatedSingle tag n ext1 ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n Nothing ext2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]

    DuplicatedMultiple tag n i ext1 ext2 ->
      let
        fname1 = showFile tag n (Just i) ext1
        fname2 = showFile tag n (Just i) ext2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]


printMultiple :: Tag -> Number -> Map.Map Index Extension -> IO ()
printMultiple tag n multMap =
  forM_ (Map.toList multMap)
    (\(i, ext) -> putStrLn $ "  * " ++ showFile tag n (Just i) ext)


showFile :: Tag -> Number -> Maybe Index -> Extension -> String
showFile tag n iopt ext =
  let
    istr =
      case iopt of
        Nothing -> ""
        Just i  -> ":" ++ show i
  in
  tag ++ "[" ++ show n ++ istr ++ "]." ++ ext

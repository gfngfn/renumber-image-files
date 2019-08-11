module Lib where

import qualified System.Directory as Dir
import Control.Monad

import Types
import qualified FileNameParser
import qualified TagMap


getFileList :: IO ()
getFileList = do
  fnames <- Dir.listDirectory "."
  printFileList fnames


printFileList :: [String] -> IO ()
printFileList fnames =
  forM_ fnames printFile


printFile :: String -> IO ()
printFile fname =
  case FileNameParser.parse fname of
    Just (FileInfo (tag, n, iopt, ext)) ->
      let
        str = showFile tag n iopt ext
      in
      putStrLn $ "* " ++ fname ++ " (" ++ str ++ ")"

    Nothing ->
      putStrLn $ "  " ++ fname


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
  foldl validateSingle ([], TagMap.empty) files


printError :: Error -> IO ()
printError err =
  case err of
    SingleAlreadyExists tag n ext1 i ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n (Just i) ext2
      in do
        putStrLn $ "! There already exists a single:"
        putStrLn $ "  * " ++ fname1
        putStrLn $ "  and thus cannot validate:"
        putStrLn $ "  * " ++ fname2

    MultipleAlreadyExists tag n _ ext2 ->
      let
        fname2 = showFile tag n Nothing ext2
      in do
        putStrLn $ "! There already exists a multiple, and should rename:"
        putStrLn $ "  * " ++ fname2

    DuplicatedSingle tag n ext1 ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n Nothing ext2
      in do
        putStrLn $ "! duplication as to extension:"
        putStrLn $ "  * " ++ fname1
        putStrLn $ "  * " ++ fname2

    DuplicatedMultiple tag n i ext1 ext2 ->
      let
        fname1 = showFile tag n (Just i) ext1
        fname2 = showFile tag n (Just i) ext2
      in do
        putStrLn $ "! duplication as to extension:"
        putStrLn $ "  * " ++ fname1
        putStrLn $ "  * " ++ fname2


showFile :: Tag -> Number -> Maybe Index -> Extension -> String
showFile tag n iopt ext =
  let
    istr =
      case iopt of
        Nothing -> ""
        Just i  -> ":" ++ show i
  in
  tag ++ "[" ++ show n ++ istr ++ "]." ++ ext

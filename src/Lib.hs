module Lib where

import qualified Data.Map.Strict as Map
import qualified System.Directory as Dir
import Control.Monad

import Types
import qualified FileNameParser
import qualified TagMap


getFileList :: IO ()
getFileList = do
  fnames <- Dir.listDirectory "."
  finfos <- parseFileNames fnames
  let (errs, _) = makeValidationMap finfos
  forM_ errs printError
--  printFileList fnames


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


parseFileNames :: [String] -> IO [FileInfo]
parseFileNames fnames =
  let
    aux :: [FileInfo] -> [String] -> IO [FileInfo]
    aux acc [] =
      return $ reverse acc

    aux acc (fname : fnametail) =
      case FileNameParser.parse fname of
        Just finfo ->
          aux (finfo : acc) fnametail

        Nothing -> do
          putStrLn $ "! cannot parse '" ++ fname ++ "'"
          aux acc fnametail
  in
  aux [] fnames


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

    MultipleAlreadyExists tag n multMap ext2 ->
      let
        fname2 = showFile tag n Nothing ext2
      in do
        putStrLn $ "! There already exists a multiple:"
        printMultiple tag n multMap
        putStrLn $ "and should rename:"
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

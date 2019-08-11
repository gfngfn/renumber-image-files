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
    Just (s, n, iopt, ext) ->
      let
        str =
          case iopt of
            Nothing -> " (" ++ s ++ "[" ++ show n ++ "]" ++ ext ++ ")"
            Just i  -> " (" ++ s ++ "[" ++ show n ++ ":" ++ show i ++ "]" ++ ext ++ ")"
      in
      putStrLn ("* " ++ fname ++ str)

    Nothing ->
      putStrLn ("  " ++ fname)


makeValidationMap :: [FileInfo] -> ([Error], TagMap.TagMap)
makeValidationMap files =
  let
    validateSingle :: ([Error], TagMap.TagMap) -> FileInfo -> ([Error], TagMap.TagMap)
    validateSingle (errAcc, tagMap) file =
      let (tag, n, iopt, ext) = file in
      case TagMap.add tag n iopt ext tagMap of
        Right tagMapNew -> (errAcc, tagMapNew)
        Left err        -> (err : errAcc, tagMap)
  in
  foldl validateSingle ([], TagMap.empty) files

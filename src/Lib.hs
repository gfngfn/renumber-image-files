module Lib where

import qualified System.Directory as Dir
import Control.Monad

import Types
import qualified FileNameParser


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

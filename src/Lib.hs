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
    Just (s, kd, ext) ->
      let
        str =
          case kd of
            Single n -> " (" ++ s ++ "[" ++ show n ++ "]" ++ ext ++ ")"
            Multiple n i -> " (" ++ s ++ "[" ++ show n ++ ":" ++ show i ++ "]" ++ ext ++ ")"
      in
      putStrLn ("* " ++ fname ++ str)

    Nothing ->
      putStrLn ("  " ++ fname)

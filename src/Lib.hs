module Lib where

import qualified System.Directory as Dir

import Types
import qualified FileNameParser


getFileList :: IO ()
getFileList = do
  fnames <- Dir.getDirectoryContents "."
  printList fnames


printList :: [String] -> IO ()
printList [] =
  return ()

printList (fname : fnames) =
  case FileNameParser.parse fname of
    Just (s, kd, ext) ->
      let
        str =
          case kd of
            Single n -> " (" ++ s ++ "[" ++ show n ++ "]" ++ ext ++ ")"
            Multiple n i -> " (" ++ s ++ "[" ++ show n ++ ":" ++ show i ++ "]" ++ ext ++ ")"
      in do
        putStrLn ("* " ++ fname ++ str)
        printList fnames

    Nothing -> do
        putStrLn ("  " ++ fname)
        printList fnames

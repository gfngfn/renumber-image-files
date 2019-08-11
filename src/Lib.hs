module Lib where

import qualified System.Directory as Dir

import Types
import qualified FileNameParser


getFileList :: IO ()
getFileList =
  Dir.getDirectoryContents "." >>= printList


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
      in
      putStrLn ("* " ++ fname ++ str) >>= \() -> printList fnames

    Nothing ->
      putStrLn ("  " ++ fname) >>= \() -> printList fnames

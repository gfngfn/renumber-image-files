module Lib
    ( getFileList
    ) where

import qualified System.Directory as Dir

getFileList :: IO ()
getFileList =
  Dir.getDirectoryContents "." >>= printList

printList :: [String] -> IO ()
printList [] = return ()
printList (s : ss) = putStrLn s >>= \() -> printList ss

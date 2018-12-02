{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getFileList
    ) where

import qualified System.Directory as Dir
import qualified Data.Text as T
import Data.Attoparsec.Text

getFileList :: IO ()
getFileList =
  Dir.getDirectoryContents "." >>= printList

printList :: [String] -> IO ()
printList [] = return ()
printList (s : ss) =
  case s of
    '.' : _ -> putStrLn ("* " ++ s) >>= \() -> printList ss
    _       -> putStrLn ("  " ++ s) >>= \() -> printList ss

{-
fileNameParser :: Parser (Text, Int)
fileNameParser = do
  s <- word
-}

word :: Parser String
word = do
  c <- letter
  s <- word
  return (c : s)

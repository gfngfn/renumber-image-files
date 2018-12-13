{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified System.Directory as Dir
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative


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

data Kind
  = Single Int
  | Multiple Int Int
  deriving Show


type Extension = String


parseDigits :: Int -> P.Parser Int
parseDigits i = do
  s <- P.count i P.digit
  return (read s :: Int)


parseExtension :: P.Parser Extension
parseExtension =
  P.char '.' *> (P.many1 P.letter <* (P.string "_large" <|> P.string ""))


parseIndex :: P.Parser (Maybe Int, Extension)
parseIndex =
  p1 <|> p2
    where
      p1 :: P.Parser (Maybe Int, Extension)
      p1 = (\s -> (Nothing, s)) <$> parseExtension

      p2 :: P.Parser (Maybe Int, Extension)
      p2 = (\n s -> (Just n, s)) <$> (P.char '_' *> parseDigits 2) <*> parseExtension


parseNumbering :: P.Parser (Char, Kind, Extension)
parseNumbering = do
  lastLetter <- P.letter
  number <- parseDigits 3
  (maybeIndex, ext) <- parseIndex
  let
    kd =
      case maybeIndex of
        Nothing -> Single number
        Just index -> Multiple number index
  return (lastLetter, kd, ext)


parseFileName :: P.Parser (String, Kind, Extension)
parseFileName =
  aux ""
    where
      aux :: String -> P.Parser (String, Kind, Extension)
      aux sacc =
        pLast <|> pMiddle
          where
            pLast :: P.Parser (String, Kind, Extension)
            pLast = (\(c, kd, ext) -> (sacc ++ [c], kd, ext)) <$> parseNumbering

            pMiddle :: P.Parser (String, Kind, Extension)
            pMiddle = do
              c <- P.letter <|> P.digit
              aux (sacc ++ [c])

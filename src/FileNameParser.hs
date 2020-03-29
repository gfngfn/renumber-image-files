{-# LANGUAGE OverloadedStrings #-}

module FileNameParser where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative

import Types


parse :: String -> Maybe FileInfo
parse fname =
  case P.parse parseFileName (T.pack fname) `P.feed` "" of
    P.Done "" finfo -> Just finfo
    _               -> Nothing


parseDigits :: Int -> P.Parser Int
parseDigits i = do
  s <- P.count i P.digit
  return (read s :: Int)


parseExtension :: P.Parser Extension
parseExtension =
  P.char '.' *> (P.many1 (P.letter <|> P.digit) <* (P.string "_large" <|> P.string ""))


parseIndex :: P.Parser (Maybe Index, Extension)
parseIndex =
  p1 <|> p2
    where
      p1 :: P.Parser (Maybe Index, Extension)
      p1 = (\s -> (Nothing, s)) <$> parseExtension

      p2 :: P.Parser (Maybe Index, Extension)
      p2 = (\n s -> (Just n, s)) <$> (P.char '_' *> parseDigits 2) <*> parseExtension


parseNumbering :: P.Parser (Char, Number, Maybe Index, Extension)
parseNumbering = do
  lastLetter <- P.letter
  number <- parseDigits 3
  (maybeIndex, ext) <- parseIndex
  return (lastLetter, number, maybeIndex, ext)


parseFileName :: P.Parser FileInfo
parseFileName =
  aux ""
    where
      aux :: String -> P.Parser FileInfo
      aux sacc =
        pLast <|> pMiddle
          where
            pLast :: P.Parser FileInfo
            pLast = (\(c, n, i, ext) -> FileInfo (sacc ++ [c], n, i, ext)) <$> parseNumbering

            pMiddle :: P.Parser FileInfo
            pMiddle = do
              c <- P.letter <|> P.digit
              aux (sacc ++ [c])

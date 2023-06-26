{-# LANGUAGE OverloadedStrings #-}

module FileNameParser where

import Data.Text qualified as T
import Data.Set qualified as Set
import Data.Attoparsec.Text qualified as P
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

parseClasses :: P.Parser Property
parseClasses =
  p1 <|> p2
    where
      p1 = (\ext -> (Set.empty, ext)) <$> parseExtension
      p2 = (\classes ext -> (Set.fromList classes, ext)) <$> (P.string "__" *> (P.sepBy1 pClass pSep)) <*> parseExtension
        where
          pClass = P.many1 P.letter
          pSep = P.char '_'

parseIndex :: P.Parser (Maybe Index, Property)
parseIndex =
  p1 <|> p2
    where
      p1 = do
        prop <- parseClasses
        pure (Nothing, prop)
      p2 = (\n prop -> (Just n, prop)) <$> (P.char '_' *> parseDigits 2) <*> parseClasses

parseNumbering :: P.Parser (Char, Number, Maybe Index, Property)
parseNumbering = do
  lastLetter <- P.letter
  number <- parseDigits 3
  (maybeIndex, prop) <- parseIndex
  return (lastLetter, number, maybeIndex, prop)

parseFileName :: P.Parser FileInfo
parseFileName =
  aux ""
    where
      aux :: String -> P.Parser FileInfo
      aux sacc =
        pLast <|> pMiddle
          where
            pLast :: P.Parser FileInfo
            pLast = do
              (c, n, i, prop) <- parseNumbering
              pure $ FileInfo (sacc ++ [c], n, i, prop)

            pMiddle :: P.Parser FileInfo
            pMiddle = do
              c <- P.letter <|> P.digit
              aux (sacc ++ [c])

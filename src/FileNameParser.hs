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


parseClasses :: P.Parser ([Class], Extension)
parseClasses =
  p1 <|> p2
    where
      p1 = (\ext -> ([], ext)) <$> parseExtension

      p2 = (,) <$> (P.string "__" *> (P.sepBy1 pClass pSep)) <*> parseExtension
        where
          pClass = P.many1 P.letter
          pSep = P.char '_'


parseIndex :: P.Parser (Maybe Index, [Class], Extension)
parseIndex =
  p1 <|> p2
    where
      p1 = do
        (classes, ext) <- parseClasses
        pure (Nothing, classes, ext)

      p2 = (\n (classes, ext) -> (Just n, classes, ext)) <$> (P.char '_' *> parseDigits 2) <*> parseClasses


parseNumbering :: P.Parser (Char, Number, Maybe Index, [Class], Extension)
parseNumbering = do
  lastLetter <- P.letter
  number <- parseDigits 3
  (maybeIndex, classes, ext) <- parseIndex
  return (lastLetter, number, maybeIndex, classes, ext)


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
              (c, n, i, classes, ext) <- parseNumbering
              pure $ FileInfo (sacc ++ [c], n, i, classes, ext)

            pMiddle :: P.Parser FileInfo
            pMiddle = do
              c <- P.letter <|> P.digit
              aux (sacc ++ [c])

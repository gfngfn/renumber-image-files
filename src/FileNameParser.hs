{-# LANGUAGE OverloadedStrings #-}

module FileNameParser where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative

import Types


parse :: String -> Maybe (String, Kind, Extension)
parse fname =
  case P.parse parseFileName (T.pack fname) `P.feed` "" of
    P.Done "" (s, kd, ext) -> Just (s, kd, ext)
    _                      -> Nothing


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

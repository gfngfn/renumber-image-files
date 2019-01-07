module Numbers where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Json
import qualified Data.Scientific as Sci


type Numbers = HashMap.HashMap T.Text Int


decodeInt :: Json.Value -> Int
decodeInt (Json.Number num) =
  case (Sci.floatingOrInteger num :: Either Double Int) of
    Right n -> n
    Left x -> round x


load :: FilePath -> IO (Maybe Numbers)
load path = do
  s <- BS.readFile path
  let maybejson = Json.decode s :: Maybe Json.Value
  case maybejson of
    Nothing ->
      return Nothing

    Just (Json.Object hashmap) ->
      return (Just (HashMap.map decodeInt hashmap))

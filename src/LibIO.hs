module LibIO where

import qualified Data.Map.Strict as Map
import qualified System.Directory as Dir
import Control.Monad

import Types


listDirectory :: String -> IO [String]
listDirectory = Dir.listDirectory


printError :: Error -> IO ()
printError err =
  case err of
    CannotParseFileName fname ->
      putStrLn $ "! Cannot parse '" ++ fname ++ "'"

    SingleAlreadyExists tag n ext1 i ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n (Just i) ext2
      in
      mapM_ putStrLn
        [ "! There already exists a single:"
        , "  * " ++ fname1
        , "  and thus cannot validate:"
        , "  * " ++ fname2
        ]

    MultipleAlreadyExists tag n multMap ext2 ->
      let
        fname2 = showFile tag n Nothing ext2
      in do
        putStrLn $ "! There already exists a multiple:"
        printMultiple tag n multMap
        putStrLn $ "  and should rename:"
        putStrLn $ "  * " ++ fname2

    DuplicatedSingle tag n ext1 ext2 ->
      let
        fname1 = showFile tag n Nothing ext1
        fname2 = showFile tag n Nothing ext2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]

    DuplicatedMultiple tag n i ext1 ext2 ->
      let
        fname1 = showFile tag n (Just i) ext1
        fname2 = showFile tag n (Just i) ext2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]


printMultiple :: Tag -> Number -> Map.Map Index Extension -> IO ()
printMultiple tag n multMap =
  forM_ (Map.toList multMap)
    (\(i, ext) -> putStrLn $ "  * " ++ showFile tag n (Just i) ext)


showFile :: Tag -> Number -> Maybe Index -> Extension -> String
showFile tag n iopt ext =
  let
    istr =
      case iopt of
        Nothing -> ""
        Just i  -> ":" ++ show i
  in
  tag ++ "[" ++ show n ++ istr ++ "]." ++ ext


printRenumberInfo :: RenumberInfo -> IO ()
printRenumberInfo renumInfo =
  let RenumberInfo (FileInfo (tag, numOld, iopt, ext), numNew) = renumInfo in
  putStrLn $ showFile tag numOld iopt ext ++ " ---> " ++ show numNew

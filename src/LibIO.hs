module LibIO where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Char qualified as Char
import Text.Printf qualified as Printf
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Control.Monad

import Types

listFiles :: FilePath -> IO [String]
listFiles dir = do
  contents <- Dir.listDirectory dir
  filterM (\content -> Dir.doesFileExist (dir </> content)) contents

printError :: Error -> IO ()
printError err =
  case err of
    CannotParseFileName fname ->
      putStrLn $ "! Cannot parse '" ++ fname ++ "'"
    SingleAlreadyExists tag n classAndExt1 i classAndExt2 ->
      let
        fname1 = showFile tag n Nothing classAndExt1
        fname2 = showFile tag n (Just i) classAndExt2
      in
      mapM_ putStrLn
        [ "! There already exists a single:"
        , "  * " ++ fname1
        , "  and thus cannot validate:"
        , "  * " ++ fname2
        ]
    MultipleAlreadyExists tag n multMap classAndExt2 ->
      let
        fname2 = showFile tag n Nothing classAndExt2
      in do
        putStrLn $ "! There already exists a multiple:"
        printMultiple tag n multMap
        putStrLn $ "  and should rename:"
        putStrLn $ "  * " ++ fname2
    DuplicatedSingle tag n classAndExt1 classAndExt2 ->
      let
        fname1 = showFile tag n Nothing classAndExt1
        fname2 = showFile tag n Nothing classAndExt2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]
    DuplicatedMultiple tag n i classAndExt1 classAndExt2 ->
      let
        fname1 = showFile tag n (Just i) classAndExt1
        fname2 = showFile tag n (Just i) classAndExt2
      in
      mapM_ putStrLn
        [ "! Duplication as to extension:"
        , "  * " ++ fname1
        , "  * " ++ fname2
        ]

printMultiple :: Tag -> Number -> Map Index Property -> IO ()
printMultiple tag n multMap =
  forM_ (Map.toList multMap)
    (\(i, classAndExt) -> putStrLn $ "  * " ++ showFile tag n (Just i) classAndExt)

showFile :: Tag -> Number -> Maybe Index -> Property -> String
showFile tag n iopt (classes, ext) =
  tag ++ showNumber n ++ showIndex iopt ++ showClasses classes ++ "." ++ ext

showNumber :: Number -> String
showNumber =
  Printf.printf "%03d"

showIndex :: Maybe Index -> String
showIndex Nothing  = ""
showIndex (Just i) = Printf.printf "_%02d" i

showClasses :: Set Class -> String
showClasses classSet =
  case Set.toList classSet of
    []      -> ""
    classes -> "__" ++ List.intercalate "_" classes

normalizeExtension :: Extension -> Extension
normalizeExtension ext =
  let extLower = List.map Char.toLower ext in
  case extLower of
    "jpeg" -> "jpg"
    "jpe"  -> "jpg"
    _      -> extLower

renameSafely :: FilePath -> FilePath -> IO ()
renameSafely fpathOld fpathNew = do
  exists <- Dir.doesFileExist fpathNew
  if exists then
    putStrLn $ "! (from '" ++ fpathOld ++ "') '" ++ fpathNew ++ "' already exists."
  else do
    putStrLn $ fpathOld ++ " ---> " ++ fpathNew
    Dir.renameFile fpathOld fpathNew

performRenumbering :: FilePath -> FilePath -> RenumberInfo -> IO ()
performRenumbering dirFrom dirTo renumInfo =
  let RenumberInfo (FileInfo (tag, numOld, iopt, (classSet, extOld)), numNew) = renumInfo in
  let extNew = normalizeExtension extOld in
  let fnameOld = showFile tag numOld iopt (classSet, extOld) in
  let fnameNew = showFile tag numNew iopt (classSet, extNew) in
  let fpathOld = dirFrom </> fnameOld in
  let fpathNew = dirTo </> fnameNew in
  if dirFrom == dirTo && numOld == numNew && extOld == extNew then
    -- putStrLn $ fpathOld ++ " (unchanged)"
    return ()
  else
    renameSafely fpathOld fpathNew

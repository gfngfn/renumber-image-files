module LibIO where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Text.Printf as Printf
import qualified System.Directory as Dir
import System.FilePath  -- in order to use </>
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
  tag ++ showNumber n ++ showIndex iopt ++ "." ++ ext


showNumber :: Number -> String
showNumber =
  Printf.printf "%03d"


showIndex :: Maybe Index -> String
showIndex Nothing  = ""
showIndex (Just i) = Printf.printf "_%02d" i


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
    -- Dir.renameFile fpathOld fpathNew


performRenumbering :: FilePath -> FilePath -> RenumberInfo -> IO ()
performRenumbering dirFrom dirTo renumInfo =
  let RenumberInfo (FileInfo (tag, numOld, iopt, extOld), numNew) = renumInfo in
  let extNew = normalizeExtension extOld in
  let fnameOld = showFile tag numOld iopt extOld in
  let fnameNew = showFile tag numNew iopt extNew in
  let fpathOld = dirFrom </> fnameOld in
  let fpathNew = dirTo </> fnameNew in
  if numOld == numNew && extOld == extNew then
    putStrLn $ fpathOld ++ " (unchanged)"
    -- return ()
  else
    renameSafely fpathOld fpathNew

module Main where

import qualified System.Directory as Dir

import qualified Lib
import qualified TagMap

main :: IO ()
main = do
  fnames <- Dir.listDirectory "."
  let (errsParse, res) = Lib.checkFileList fnames
  case res of
    Left errsDup -> do
      mapM_ Lib.printError (errsParse ++ errsDup)
      putStrLn "Conflicts of numbers should be fixed manually."

    Right tagMap -> do
      mapM_ Lib.printError errsParse
      let (renumInfos, _numNextMap) = TagMap.getRenumberInfos tagMap
      putStrLn "Success."
      mapM_ Lib.printRenumberInfo renumInfos

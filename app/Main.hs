module Main where

import qualified Lib
import qualified LibIO
import qualified TagMap


main :: IO ()
main = do
  fnames <- LibIO.listDirectory "."
  let (errsParse, res) = Lib.checkFileList fnames
  case res of
    Left errsDup -> do
      mapM_ LibIO.printError (errsParse ++ errsDup)
      putStrLn "Conflicts of numbers should be fixed manually."

    Right tagMap -> do
      mapM_ LibIO.printError errsParse
      let (renumInfos, _numNextMap) = TagMap.getRenumberInfos tagMap
      putStrLn "No conflicts found."
      mapM_ LibIO.printRenumberInfo renumInfos

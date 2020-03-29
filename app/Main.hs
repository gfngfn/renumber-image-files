module Main where

import qualified System.Directory as Dir

import qualified Lib

main :: IO ()
main = do
  fnames <- Dir.listDirectory "."
  let (errsParse, res) = Lib.checkFileList fnames
  case res of
    Left errsDup -> do
      mapM_ Lib.printError (errsParse ++ errsDup)
      putStrLn "Conflicts of numbers should be fixed manually."

    Right _tagMap -> do
      mapM_ Lib.printError errsParse
      putStrLn "Success."

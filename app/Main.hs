module Main where

import System.Directory as Dir
import System.Environment as Environment
import Data.List as List
import Data.Map.Strict as Map

import Types
import qualified Lib
import qualified LibIO
import qualified TagMap


data Mode
  = DryRun FilePath
  | Run FilePath FilePath
  | Invalid


parseArgs :: [String] -> Mode
parseArgs ["--check", dir]                = DryRun dir
parseArgs ["--run", dirSource, dirTarget] = Run dirSource dirTarget
parseArgs _                               = Invalid


traverseDirectory :: NextNumberMap -> FilePath -> IO (Either () ([RenumberInfo], NextNumberMap))
traverseDirectory numPrevMap dir = do
  fnames <- LibIO.listFiles dir
  let (errsParse, res) = Lib.checkFileList fnames
  case res of
    Left errsDup -> do
      mapM_ LibIO.printError (errsParse ++ errsDup)
      putStrLn "Conflicts of numbers should be fixed manually."
      return $ Left ()

    Right tagMap -> do
      mapM_ LibIO.printError errsParse
      let (renumInfos, numNextMap) = TagMap.getRenumberInfos numPrevMap tagMap
      putStrLn "No conflicts found."
      return $ Right (renumInfos, numNextMap)


main :: IO ()
main = do
  args <- Environment.getArgs
  case parseArgs args of
    Invalid ->
      putStrLn "invalid command line arguments."

    DryRun dir -> do
      _ <- traverseDirectory Map.empty dir
      return ()

    Run dirSource dirTarget -> do
      putStrLn $ "Traversing the target directory '" ++ dirTarget ++ "' ..."
      resTarget <- traverseDirectory Map.empty dirTarget
      case resTarget of
        Left () -> do
          putStrLn "Found errors in the target directory. Stop."
          return ()

        Right (renumInfosTarget, numNextMap) -> do
          putStrLn $ "Traversing the source directory '" ++ dirSource ++ "' ..."
          resSource <- traverseDirectory numNextMap dirSource
          case resSource of
            Left () -> do
              putStrLn "Found errors in the source directory. Stop."
              return ()

            Right (renumInfosSource, _) -> do
              putStrLn $ "Renaming " ++ show (List.length renumInfosSource) ++ " files ..."
              mapM_ (LibIO.performRenumbering dirTarget dirTarget) renumInfosTarget
              mapM_ (LibIO.performRenumbering dirSource dirTarget) renumInfosSource
              putStrLn "End."

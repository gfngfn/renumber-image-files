module Main where

import qualified System.Directory as Dir
import qualified System.FilePath.Posix as Posix
import qualified System.Environment as Environment
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import System.FilePath  -- in order to use </>

import Types
import qualified Lib
import qualified LibIO
import qualified TagMap


data Mode
  = DryRun FilePath
  | Normalize FilePath
  | SumUp FilePath
  | Run FilePath FilePath
  | Invalid


parseArgs :: [String] -> Mode
parseArgs ["--check", dir]                = DryRun dir
parseArgs ["--normalize", dir]            = Normalize dir
parseArgs ["--sum-up", dir]               = SumUp dir
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


makePathAbsolute :: FilePath -> FilePath -> FilePath
makePathAbsolute dirCurrent dir =
  if Posix.isRelative dir then dirCurrent </> dir else dir


main :: IO ()
main = do
  dirCurrent <- Dir.getCurrentDirectory
  args <- Environment.getArgs
  case parseArgs args of
    Invalid ->
      putStrLn "invalid command line arguments."

    DryRun dir0 -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      _ <- traverseDirectory Map.empty dir
      return ()

    SumUp dir0 -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      res <- traverseDirectory Map.empty dir
      case res of
        Left () ->
          putStrLn "Found errors in the directory. Stop."

        Right (_, numNextMap) ->
          let numAssoc = List.sortBy (\(_, n1) (_, n2) -> compare n2 n1) (Map.toList numNextMap) in
          mapM_ (\(tag, num) ->
            putStrLn $ "* " ++ tag ++ ": " ++ show (num - 1)
          ) numAssoc

    Normalize dir0 -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      res <- traverseDirectory Map.empty dir
      case res of
        Left () ->
          putStrLn "Found errors in the directory. Stop."

        Right (renumInfos, _) -> do
          putStrLn $ "Renaming " ++ show (List.length renumInfos) ++ " files ..."
          mapM_ (LibIO.performRenumbering dir dir) renumInfos
          putStrLn "End."

    Run dirSource0 dirTarget0 -> do
      let dirSource = makePathAbsolute dirCurrent dirSource0
      let dirTarget = makePathAbsolute dirCurrent dirTarget0
      putStrLn $ "Traversing the target directory '" ++ dirTarget ++ "' ..."
      resTarget <- traverseDirectory Map.empty dirTarget
      case resTarget of
        Left () ->
          putStrLn "Found errors in the target directory. Stop."

        Right (renumInfosTarget, numNextMap) -> do
          putStrLn $ "Traversing the source directory '" ++ dirSource ++ "' ..."
          resSource <- traverseDirectory numNextMap dirSource
          case resSource of
            Left () ->
              putStrLn "Found errors in the source directory. Stop."

            Right (renumInfosSource, _) -> do
              putStrLn $ "Renaming " ++ show (List.length renumInfosSource) ++ " files ..."
              mapM_ (LibIO.performRenumbering dirTarget dirTarget) renumInfosTarget
              mapM_ (LibIO.performRenumbering dirSource dirTarget) renumInfosSource
              putStrLn "End."

module Main where

import qualified System.Directory as Dir
import qualified System.FilePath.Posix as Posix
import qualified System.Environment as Environment
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.FilePath  -- in order to use </>

import Types
import qualified Lib
import qualified LibIO
import qualified TagMap


data Options = Options
  { verbose :: Bool
  , safe    :: Bool
  }


data Mode
  = ShowHelp
  | DryRun FilePath
  | Normalize FilePath
  | SumUp FilePath
  | Run (Maybe (Set.Set String)) FilePath FilePath


type ParsedMode = (Maybe Mode, Options)


defaultMode :: ParsedMode
defaultMode =
  let
    opts = Options
      { verbose = True
      , safe    = True
      }
  in
  (Nothing, opts)


parseArgs :: ParsedMode -> [String] -> ParsedMode
parseArgs maybeMode []                               = maybeMode
parseArgs (Nothing, opts) ("--help" : xs)            = parseArgs (Just ShowHelp, opts) xs
parseArgs (Nothing, opts) ("--check" : dir : xs)     = parseArgs (Just (DryRun dir), opts) xs
parseArgs (Nothing, opts) ("--normalize" : dir : xs) = parseArgs (Just (Normalize dir), opts) xs
parseArgs (Nothing, opts) ("--sum-up" : dir : xs)    = parseArgs (Just (SumUp dir), opts) xs

parseArgs (Nothing, opts) ("--run" : dirSource : dirTarget : xs) =
  parseArgs (Just (Run Nothing dirSource dirTarget), opts) xs

parseArgs (Nothing, opts) ("--run-only" : tagsString : dirSource : dirTarget : xs) =
  let tags = Set.fromList (ListSplit.splitOn "," tagsString) in
  parseArgs (Just (Run (Just tags) dirSource dirTarget), opts) xs

parseArgs (maybeMode, opts) ("--concise" : xs) = parseArgs (maybeMode, opts { verbose = False }) xs
parseArgs (maybeMode, opts) ("--do" : xs)      = parseArgs (maybeMode, opts { safe = False }) xs
parseArgs (_, opts) _                          = (Nothing, opts)


traverseDirectory :: Options -> Maybe (Set.Set String) -> NextNumberMap -> FilePath -> IO (Either () ([RenumberInfo], NextNumberMap))
traverseDirectory opts tagRestriction numPrevMap dir = do
  fnames <- LibIO.listFiles dir
  let (errsParseOriginal, res) = Lib.checkFileList tagRestriction fnames
  let errsParse = if verbose opts then errsParseOriginal else []
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


printHelp :: IO ()
printHelp = do
  putStrLn "* renum-files-exe --check DIR"
  putStrLn "* renum-files-exe --normalize DIR"
  putStrLn "* renum-files-exe --sum-up DIR"
  putStrLn "* renum-files-exe --run SOURCE TARGET"
  putStrLn "* renum-files-exe --run-only PREFIX SOURCE TARGET"
  putStrLn "Options:"
  putStrLn "* --concise"
  putStrLn "* --do (execute renaming for more than 100 files)"


main :: IO ()
main = do
  dirCurrent <- Dir.getCurrentDirectory
  args <- Environment.getArgs
  let (maybeMode, opts) = parseArgs defaultMode args
  case maybeMode of
    Nothing -> do
      putStrLn "Invalid command line arguments. Usage:"
      printHelp

    Just ShowHelp ->
      printHelp

    Just (DryRun dir0) -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      _ <- traverseDirectory opts Nothing Map.empty dir
      return ()

    Just (SumUp dir0) -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      res <- traverseDirectory opts Nothing Map.empty dir
      case res of
        Left () ->
          putStrLn "Found errors in the directory. Stop."

        Right (_, numNextMap) ->
          let numAssoc = List.sortBy (\(_, n1) (_, n2) -> compare n2 n1) (Map.toList numNextMap) in
          mapM_ (\(tag, num) ->
            putStrLn $ "* " ++ tag ++ ": " ++ show (num - 1)
          ) numAssoc

    Just (Normalize dir0) -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      res <- traverseDirectory opts Nothing Map.empty dir
      case res of
        Left () ->
          putStrLn "Found errors in the directory. Stop."

        Right (renumInfos, _) -> do
          putStrLn $ "Renaming " ++ show (List.length renumInfos) ++ " files ..."
          mapM_ (LibIO.performRenumbering dir dir) renumInfos
          putStrLn "End."

    Just (Run maybeTags dirSource0 dirTarget0) -> do
      let dirSource = makePathAbsolute dirCurrent dirSource0
      let dirTarget = makePathAbsolute dirCurrent dirTarget0
      putStrLn $ "Traversing the target directory '" ++ dirTarget ++ "' ..."
      resTarget <- traverseDirectory opts Nothing Map.empty dirTarget
      case resTarget of
        Left () ->
          putStrLn "Found errors in the target directory. Stop."

        Right (renumInfosTarget, numNextMap) -> do
          putStrLn $ "Traversing the source directory '" ++ dirSource ++ "' ..."
          resSource <- traverseDirectory opts maybeTags numNextMap dirSource
          case resSource of
            Left () ->
              putStrLn "Found errors in the source directory. Stop."

            Right (renumInfosSource, _) -> do
              let numFiles = List.length renumInfosSource
              if numFiles > 100 && safe opts then
                putStrLn $ "Will perform remaning " ++ show numFiles ++ " files; rerun with --do option for renaming more than 100 files. Stop."
              else do
                putStrLn $ "Renaming " ++ show numFiles ++ " files ..."
                mapM_ (LibIO.performRenumbering dirTarget dirTarget) renumInfosTarget
                mapM_ (LibIO.performRenumbering dirSource dirTarget) renumInfosSource
                putStrLn "End."

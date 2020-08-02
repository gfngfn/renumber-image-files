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


data Options = Options
  { verbose :: Bool
  , safe    :: Bool
  }


data Mode
  = DryRun FilePath
  | Normalize FilePath
  | SumUp FilePath
  | Run FilePath FilePath


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
parseArgs parsedMode []                                            = parsedMode
parseArgs (Nothing, opts) ("--check" : dir : tail)                 = parseArgs (Just (DryRun dir), opts) tail
parseArgs (Nothing, opts) ("--normalize" : dir : tail)             = parseArgs (Just (Normalize dir), opts) tail
parseArgs (Nothing, opts) ("--sum-up" : dir : tail)                = parseArgs (Just (SumUp dir), opts) tail
parseArgs (Nothing, opts) ("--run" : dirSource : dirTarget : tail) = parseArgs (Just (Run dirSource dirTarget), opts) tail
parseArgs (maybeMode, opts) ("--concise" : tail)                   = parseArgs (maybeMode, opts { verbose = False }) tail
parseArgs (maybeMode, opts) ("--do" : tail)                        = parseArgs (maybeMode, opts { safe = False }) tail
parseArgs (_, opts) _                                              = (Nothing, opts)


traverseDirectory :: Options -> NextNumberMap -> FilePath -> IO (Either () ([RenumberInfo], NextNumberMap))
traverseDirectory opts numPrevMap dir = do
  fnames <- LibIO.listFiles dir
  let (errsParseOriginal, res) = Lib.checkFileList fnames
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


main :: IO ()
main = do
  dirCurrent <- Dir.getCurrentDirectory
  args <- Environment.getArgs
  let (maybeMode, opts) = parseArgs defaultMode args
  case maybeMode of
    Nothing ->
      putStrLn "invalid command line arguments."

    Just (DryRun dir0) -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      _ <- traverseDirectory opts Map.empty dir
      return ()

    Just (SumUp dir0) -> do
      let dir = makePathAbsolute dirCurrent dir0
      putStrLn $ "Traversing '" ++ dir ++ "' ..."
      res <- traverseDirectory opts Map.empty dir
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
      res <- traverseDirectory opts Map.empty dir
      case res of
        Left () ->
          putStrLn "Found errors in the directory. Stop."

        Right (renumInfos, _) -> do
          putStrLn $ "Renaming " ++ show (List.length renumInfos) ++ " files ..."
          mapM_ (LibIO.performRenumbering dir dir) renumInfos
          putStrLn "End."

    Just (Run dirSource0 dirTarget0) -> do
      let dirSource = makePathAbsolute dirCurrent dirSource0
      let dirTarget = makePathAbsolute dirCurrent dirTarget0
      putStrLn $ "Traversing the target directory '" ++ dirTarget ++ "' ..."
      resTarget <- traverseDirectory opts Map.empty dirTarget
      case resTarget of
        Left () ->
          putStrLn "Found errors in the target directory. Stop."

        Right (renumInfosTarget, numNextMap) -> do
          putStrLn $ "Traversing the source directory '" ++ dirSource ++ "' ..."
          resSource <- traverseDirectory opts numNextMap dirSource
          case resSource of
            Left () ->
              putStrLn "Found errors in the source directory. Stop."

            Right (renumInfosSource, _) -> do
              let numFiles = List.length renumInfosSource
              if numFiles >= 100 && safe opts then
                putStrLn $ "Will perform remaning " ++ show numFiles ++ " files; rerun with --do option for renaming more than 100 files. Stop."
              else do
                putStrLn $ "Renaming " ++ show numFiles ++ " files ..."
                mapM_ (LibIO.performRenumbering dirTarget dirTarget) renumInfosTarget
                mapM_ (LibIO.performRenumbering dirSource dirTarget) renumInfosSource
                putStrLn "End."

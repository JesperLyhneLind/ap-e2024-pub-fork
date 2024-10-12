module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      s' <- readDB db
      case s' of
        Left err -> pure $ Left err
        Right state -> runEvalIO' r db (k state)
    runEvalIO' r db (Free (StatePutOp s m)) = do
      writeDB db s
      runEvalIO' r db m
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      res1 <- runEvalIO' r db m1
      case res1 of
        Left _ -> runEvalIO' r db m2 -- Running m2 if m1 fails
        Right val -> pure $ Right val -- Returning the result of m1 if it succeeds
    runEvalIO' r db (Free (KvGetOp key k)) = do
      dbState <- readDB db
      case dbState of
        Left err -> pure $ Left err
        Right state -> 
          case lookup key state of
            Nothing -> do
              putStr $ "Invalid key: " ++ show key ++ ". Enter a replacement: "
              input <- prompt ""
              case readVal input of
                Nothing -> pure $ Left ("Invalid input: " ++ input)
                Just val -> runEvalIO' r db (k val)
            Just key' -> runEvalIO' r db (k key')
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      dbState <- readDB db
      case dbState of
        Left err -> pure $ Left err
        Right state -> do
          let dbState' = (key, val) : filter ((/= key) . fst) state -- removing existing assoctiation and adding new
          writeDB db dbState'
          runEvalIO' r db m
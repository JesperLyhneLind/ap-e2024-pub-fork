module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) =
      let (ps1, res1) = runEval' r s m1
      in case res1 of
        Left _ -> runEval' r s m2 -- Running m2 if m1 fails
        Right val -> (ps1, Right val) -- Returning the result of m1 if it succeeds
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> ([],Left $ "Key not found" ++ show key)
        Just key' -> runEval' r s (k key')
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s -- removing existing assoctiation and adding new
      in runEval' r s' m
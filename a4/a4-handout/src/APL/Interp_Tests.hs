module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, tryCatchOpTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      -- 
      testCase "evalKvGetPure" $
        runEval (evalKvGet $ ValInt 0)
          @?= ([], Left "Key not found: ValInt 0"),
      -- 
      testCase "Put and get" $ do
        let put0 = KvPutOp (ValInt 0) (ValInt 1)
        let get0 = Free $ KvGetOp (ValInt 0) $ \val -> pure val
        runEval (Free $ put0 get0)
          @?= ([],Right (ValInt 1))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
      testCase "print 2" $ do
          (out, res) <-
            captureIO [] $
              evalIO' $
                Print "This is also 1" $
                  Print "This is 1" $
                    CstInt 1
          (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
      -- 
      testCase "Missing key - valid input" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $ KvGetOp (ValInt 0) $ \val -> pure val
        res @?= Right (ValInt 1),
      -- 
      testCase "Missing key - invalid input" $ do
        (_, res) <-
          captureIO ["dd"] $
            runEvalIO $
              Free $ KvGetOp (ValInt 0) $ \val -> pure val
        res @?= Left "Invalid input: dd"
    ]

tryCatchOpTests :: TestTree
tryCatchOpTests =
  testGroup
    "TryCatchOp Effect"
    [ testCase "Test 1" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure "Success!"))
          @?= ([], Right "Success!"),
      -- 
      testCase "Test 2" $
        let divZero = CstInt 1 `Div` CstInt 0
        in runEval (eval $ TryCatch (CstInt 5) divZero)
          @?= ([], Right $ ValInt 5),
      -- 
      testCase "Test 3" $
        let divZero = CstInt 1 `Div` CstInt 0
            badEql = CstInt 0 `Eql` CstBool True
        in do
            result <- evalIO' $ TryCatch badEql divZero
            result @?= Left "Division by zero",
      -- 
      testCase "Simple success" $
        runEval
          (Free $ TryCatchOp (pure "No failure") (pure "Fallback"))
          @?= ([], Right "No failure"),
      -- 
      testCase "Nested TryCatch" $
        let action = catch
              (catch
                 (failure "First failure")
                 (failure "Second failure")
              )
              (pure $ ValInt 100)
        in runEval action @?= ([], Right $ ValInt 100),
      -- 
      testCase "No fallback with valid division" $
        let validDiv = CstInt 10 `Div` CstInt 2
        in runEval (eval $ TryCatch (Add (CstInt 5) validDiv) (CstInt 100))
          @?= ([], Right $ ValInt 10)
    ]
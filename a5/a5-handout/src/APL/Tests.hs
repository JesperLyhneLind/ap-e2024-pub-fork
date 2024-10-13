module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL, keywords)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , frequency
  , elements
  )

instance Arbitrary Exp where
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [ CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vs =
  do
  var <- genVar -- generate unique names for new variables
  frequency
    [ (10, CstInt <$> arbitrary)
    , (10, CstBool <$> arbitrary)
    , (10, Add <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, Sub <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, Mul <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, Div <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, Pow <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, Eql <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, If <$> genExp thirdSize vs <*> genExp thirdSize vs <*> genExp thirdSize vs)
    , (5, if null vs then Var <$> genVar else Var <$> elements vs) -- uses existing variable, if vs is not empty
    , (20, Let <$> pure var <*> genExp halfSize (var : vs) <*> genExp halfSize (var : vs)) -- variable added to scope
    , (20, Lambda <$> pure var <*> genExp (size - 1) (var : vs)) -- variable added to scope
    , (10, Apply <$> genExp halfSize vs <*> genExp halfSize vs)
    , (10, TryCatch <$> genExp halfSize vs <*> genExp halfSize vs)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

genVar :: Gen VName
genVar = do
  char1 <- elements ['a' .. 'z'] -- first char lowercase
  chars <- sequenceA $ replicate 3 (elements (['a' .. 'z'] ++ ['0' .. '9'])) -- 3 subsequent lowercase or integer
  let possibleVars = take 3 (iterate init (char1 : chars))  -- 2, 3, and 4 character names
  name <- elements possibleVars
  if name `elem` keywords
    then genVar
    else pure name

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted expr =
  let printedExp = printExp expr
  in case parseAPL "" printedExp of
    Left _ -> False
    Right newexp -> expr == newexp

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ 
    -- Remove Print for proper testing of others
    ("expCoverage", property expCoverage),
    ("onlyCheckedErrors", property onlyCheckedErrors),
    ("parsePrinted", property parsePrinted)
  ]

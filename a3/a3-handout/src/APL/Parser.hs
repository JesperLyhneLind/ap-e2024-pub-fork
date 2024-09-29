module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

-- Added ML
lPrint :: Parser String
lPrint= lexeme $ many (satisfy isAlpha)

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]


pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

-- Added ML
pKExp :: Parser Exp
pKExp =
  choice
    [ KvGet
        <$> (lKeyword "get" *> pAtom),
      KvPut
        <$> (lKeyword "put" *> pAtom)
        <*> pAtom,
      do
        lKeyword "print"
        lString "\""
        x <- lPrint
        lString "\""
        y <- pAtom
        pure $ Print x y,
      pAtom
    ]

-- Added ML
pFExp :: Parser Exp
pFExp = pKExp >>= chain
  where
    chain x =
      choice
        [ do
            space
            y <- pKExp
            chain $ Apply x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda <$> (lString "\\" *> lVName <* lString "->") <*> pExp,
      Let <$> (lKeyword "let" *> lVName) <*> (lString "=" *> pExp) <*> (lKeyword "in" *> pExp),
      TryCatch <$> (lKeyword "try" *> pExp) <*> (lKeyword "catch" *> pExp),
      pFExp
    ]

-- Added ML
pHExp :: Parser Exp
pHExp = pLExp >>= chain
  where
    chain x = 
      choice
        [ do
            lString "**"
            y <- pHExp
            chain $ Pow x y,
          pure x  
        ]

pExp1 :: Parser Exp
pExp1 = pHExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pHExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pHExp
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

-- Added ML
pExp :: Parser Exp
pExp = pExp0 >>= chain
  where
    chain x = 
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
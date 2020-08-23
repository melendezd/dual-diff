{-# LANGUAGE LambdaCase, RankNTypes #-}

module Parser (eval, parseExpr) where

import Text.ParserCombinators.Parsec
import Text.Read (read)
import Numeric (readFloat)
import Control.Monad (join)

data Expr a = Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Pow (Expr a) (Expr a)
            | Neg (Expr a)
            | Exp (Expr a)
            | Sin (Expr a)
            | Cos (Expr a)
            | Tan (Expr a)
            | Asin (Expr a)
            | Acos (Expr a)
            | Atan (Expr a)
            | Sinh (Expr a)
            | Cosh (Expr a)
            | Tanh (Expr a)
            | Asinh (Expr a)
            | Acosh (Expr a)
            | Atanh (Expr a)
            | Var
            | Lit a
            deriving (Show, Eq)

-- Convert expression to function --
eval :: RealFrac a => Expr a -> (forall b. Floating b => b -> b)
eval = \case
  Lit x -> const (fromRational . toRational $ x)
  Var -> id
  Add e1 e2 -> (+) <$> eval e1 <*> eval e2
  Sub e1 e2 -> (-) <$> eval e1 <*> eval e2
  Mul e1 e2 -> (*) <$> eval e1 <*> eval e2
  Div e1 e2 -> (/) <$> eval e1 <*> eval e2
  Pow e1 e2 -> (**) <$> eval e1 <*> eval e2
  Neg e -> negate <$> eval e
  Exp e -> exp <$> eval e
  Sin e -> sin <$> eval e
  Cos e -> cos <$> eval e
  Tan e -> tan <$> eval e
  Asin e -> asin <$> eval e
  Acos e -> acos <$> eval e
  Atan e -> atan <$> eval e
  Sinh e -> sinh <$> eval e
  Cosh e -> cosh <$> eval e
  Tanh e -> tanh <$> eval e
  Asinh e -> asinh <$> eval e
  Acosh e -> acosh <$> eval e
  Atanh e -> atanh <$> eval e

-- Literals and Variables --

--digit = oneOf "0123456789"
number :: RealFrac a => GenParser Char st a
number = fst . head . readFloat <$> many1 (oneOf "0123456789.")

literal :: RealFrac a => GenParser Char st (Expr a)
literal = Lit <$> number

variable :: GenParser Char st (Expr a)
variable = char 'x' >> return Var

atom :: RealFrac a => GenParser Char st (Expr a)
atom = possiblyNeg $ literal <|> variable

-- Operations and Functions --

addOp = (char '+' >> return Add)
    <|> (char '-' >> return Sub)
mulOp = (char '*' >> return Mul)
    <|> (char '/' >> return Div)
powOp = char '^' >> return Pow

funcOp = try (string "sinh" >> return Sinh) <|>
  try (string "cosh" >> return Cosh) <|>
  try (string "tanh" >> return Tanh) <|>
  try (string "asinh" >> return Asinh) <|>
  try (string "acosh" >> return Acosh) <|>
  try (string "atanh" >> return Atanh) <|>
  try (string "sin" >> return Sin) <|>
  try (string "cos" >> return Cos) <|>
  try (string "tan" >> return Tan) <|>
  try (string "asin" >> return Asin) <|>
  try (string "acos" >> return Acos) <|>
  try (string "atan" >> return Atan) <|>
  try (string "exp" >> return Exp) 

funcTerm :: RealFrac a => GenParser Char st (Expr a)
funcTerm = funcOp <*> parenExpr


-- Expressions --

expr :: RealFrac a => GenParser Char st (Expr a)
expr = addTerm `chainl1` addOp

addTerm :: RealFrac a => GenParser Char st (Expr a)
addTerm = mulTerm `chainl1` mulOp

mulTerm :: RealFrac a => GenParser Char st (Expr a)
mulTerm = powTerm `chainr1` powOp

powTerm :: RealFrac a => GenParser Char st (Expr a)
powTerm = possiblyNeg (funcTerm <|> parenExpr <|> atom)

parenExpr :: RealFrac a => GenParser Char st (Expr a)
parenExpr = possiblyNeg (parens expr)


-- Utility --

possiblyNeg :: GenParser Char st (Expr a) -> GenParser Char st (Expr a)
possiblyNeg parser = try (Neg <$> (char '-' >> parser))
  <|> parser

parens :: GenParser Char st a -> GenParser Char st a
parens = between (char '(') (char ')')

-- Export ---

parseExpr :: RealFrac a => String -> Either ParseError (Expr a)
parseExpr = parse expr "error" . removeSpaces
  where
    removeSpaces = join . map replace
    replace ' ' = []
    replace c = [c]

module Language.Parser
    ( ast
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfInput, skipSpace)
import Language.AST (BinOp(..), Expr(..))

ast :: Parser Expr
ast = expr <* skipSpace <* endOfInput

expr :: Parser Expr
expr = mulExpr

mulExpr :: Parser Expr
mulExpr = (prefix BOp <$> addExpr <*> mulOp <*> mulExpr)
    <|> addExpr

mulOp :: Parser BinOp
mulOp = (pure Mul <* token '*')
    <|> (pure Div <* token '/')

addExpr :: Parser Expr
addExpr = (prefix BOp <$> term <*> addOp <*> addExpr)
    <|> term

addOp :: Parser BinOp
addOp = (pure Add <* token '+')
    <|> (pure Sub <* token '-')

term :: Parser Expr
term = int
    <|> (token '(' *> expr <* token ')')

int :: Parser Expr
int = I <$> decimal

token :: Char -> Parser Char
token c = skipSpace *> char c

prefix :: (a -> b -> c -> d) -> (b -> a -> c -> d)
prefix f b a c = f a b c

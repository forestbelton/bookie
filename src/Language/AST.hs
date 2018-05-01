module Language.AST
    ( BinOp(..)
    , Expr(..)
    ) where

import Data.Word (Word32)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    deriving (Show)

data Expr
    = I Word32
    | BOp BinOp Expr Expr
    deriving (Show)

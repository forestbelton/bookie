module Language.CodeGen
    ( codeGen
    ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Monoid

import EVM.Insn
import Language.AST (Expr(..), BinOp(..))

compileOp :: BinOp -> Builder
compileOp op = encodeInsn $ case op of
    Add -> IAdd
    Sub -> ISub
    Mul -> IMul
    Div -> IDiv

compileExpr :: Expr -> Builder
compileExpr (I x)        = encodeInsn $ IPush 4 (word32BE x)
compileExpr (BOp op l r) = compileExpr r <> compileExpr l <> compileOp op

codeGen :: Expr -> B.ByteString
codeGen = toLazyByteString . lazyByteStringHex . toLazyByteString . (<> returnTop) . compileExpr

returnTop :: Builder
returnTop = foldMap encodeInsn code
    where code = [ IPush 1 (word8 0)
                 , IMStore
                 , IPush 1 (word8 32)
                 , IPush 1 (word8 0)
                 , IReturn
                 ]

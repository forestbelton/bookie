module Main where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import System.Environment (getArgs)

import Language.Parser (ast)
import Language.CodeGen (codeGen)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then error "must pass program as argument"
        else return ()

    let (source:_) = args
    putStrLn $ case parseOnly ast (pack source) of
        Left err  -> "parse error: " ++ err
        Right ast -> unpack $ codeGen ast

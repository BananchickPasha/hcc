module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map as M

import Parser.Lexemes
import Parser.CLike
import Translator.First

main = do
  str <- readFile "test/test.c"
  let ast = parse parseMany "" str
  print ast
  let code = case generateCode str of
               Left _ -> ""
               Right res -> res
  putStrLn ""
  putStrLn "---------CODE---------"
  putStrLn code
  writeFile "test/res.s" code

generateCode :: String -> Either ParseError String
generateCode str = do
  ast <- parse parseMany "" str
  return $ trAST ast





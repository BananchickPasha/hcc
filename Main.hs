module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as Token

import Parser.Lexemes
import Parser.CLike
import Translator.First

main = do
  let code = case generateCode of
               Left _ -> ""
               Right res -> res
  writeFile "test/res.s" code

generateCode :: Either ParseError String
generateCode = do
  ast <- parse parseMany "" "int main () { return (6-2)/2;}"
  return $ trStatement $ head ast





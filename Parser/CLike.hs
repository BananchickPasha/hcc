module Parser.CLike where
import AST
import Parser.Lexemes
import Parser.CExpr


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as Token


statements = funcStm <|> returnStm <|> assignStm
parseMany :: Parser [Statement]
parseMany = many1 statements

funcStm :: Parser Statement
funcStm = do
  void $ string "int"
  fname <- many1 $ satisfy isName
  inScope '(' whitespace
  blocks <- inScope '{' parseMany
  return $ Function fname blocks

returnStm :: Parser Statement
returnStm = do
  void $ string "return" <* whitespace
  expr <- parseAnyExpr <* whitespace
  void $ char ';' <* whitespace
  return $ Return expr

assignStm :: Parser Statement
assignStm = do
  void $ string "int"
  varName <- many1 $ satisfy isName
  void $ string "="
  expr <- constIntExpr
  return $ Assign varName expr



isName :: Char -> Bool
isName '(' = False
isName ')' = False
isName _   = True

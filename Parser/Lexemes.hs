module Parser.Lexemes 
  (whitespace, inScope, inScopes, reserved, reservedChar)
  where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (void)


whitespace :: Parser ()
whitespace = void . many $ oneOf " \n\t"

inScopes :: Char -> Char -> Parser a -> Parser a
inScopes begin end parser = do
  void $ char begin <* whitespace
  res <- parser
  void $ char end <* whitespace
  return res

inScope :: Char -> Parser a -> Parser a
inScope '{' = inScopes '{' '}'
inScope '(' = inScopes '(' ')'
inScope a = inScopes a a


reserved :: String -> Parser ()
reserved str = try $ whitespace <* string str <* whitespace
reservedChar :: Char -> Parser ()
reservedChar ch = try $ whitespace <* char ch <* whitespace

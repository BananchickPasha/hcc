module Parser.CLike where
import           AST
import           Parser.Lexemes
import           Parser.CExpr


import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Expr
import           Control.Monad
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token


statements = choice $ map try [funcStm, returnStm, declStm, assignStm, ifStm, blockStm, whileLoopStm]
parseMany :: Parser [Statement]
parseMany = many1 statements

funcStm :: Parser Statement
funcStm = do
  reserved "int"
  fname <- var
  inScope '(' whitespace
  blocks <- inScope '{' parseMany
  return $ Function fname blocks

returnStm :: Parser Statement
returnStm = do
  reserved "return"
  expr <- parseAnyExpr <* whitespace
  reservedChar ';'
  return . Return $ Just expr

declStm :: Parser Statement
declStm = do
  reserved "int"
  varName <- var
  expr <- Just <$> (reservedChar '=' >> parseAnyExpr) <|> return Nothing
  reservedChar ';'
  return $ Decl varName expr

assignStm :: Parser Statement
assignStm = do
  varName <- var
  expr <- reservedChar '=' >> parseAnyExpr
  reservedChar ';'
  return $ Assign varName expr

blockStm :: Parser Statement
blockStm = Block <$> inScope '{' parseMany

whileLoopStm :: Parser Statement
whileLoopStm = do
  reserved "while"
  expr  <- inScope '(' parseAnyExpr
  stm   <- statements
  return $ WhileLoop expr stm

ifStm :: Parser Statement
ifStm = do
  reserved "if"
  expr  <- inScope '(' parseAnyExpr
  stms  <- statements
  elifs <- Just <$> (reserved "else" >> statements) <|> return Nothing
  return $ If expr stms elifs



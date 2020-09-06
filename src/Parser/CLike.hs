module Parser.CLike where
import           AST
import           Parser.Lexemes
import           Parser.CExpr


import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Expr
import           Control.Monad
import           Data.Functor
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token


statements = choice $ map try $ [brContStms, funcStm, returnStm, declStm, ifStm, blockStm, whileLoopStm, exprStm]
parseMany :: Parser [Statement]
parseMany = many statements

funcStm :: Parser Statement
funcStm = do
  reserved "int"
  fname <- var
  params <- inScope '(' $ (reserved "int" >> var) `sepBy` char ','
  Function fname params <$> statements

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

exprStm :: Parser Statement
exprStm = do
  expr <- parseAnyExpr
  reservedChar ';'
  return $ Expr expr
blockStm :: Parser Statement
blockStm = Block <$> inScope '{' parseMany

brContStms :: Parser Statement
brContStms = (Break <$ reserved "break" <|> Continue <$ reserved "continue") <* reservedChar ';'
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



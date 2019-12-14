module Parser.CExpr where
import           AST
import           Parser.Lexemes

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Control.Monad
import           Control.Monad.Combinators.Expr
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token



parseManyExpr :: Parser [Expr]
parseManyExpr = many1 parseAnyExpr

parseAnyExpr :: Parser Expr
parseAnyExpr = flip makeExprParser operators $ inScope '(' parseAnyExpr <|> constIntExpr

constIntExpr :: Parser Expr
constIntExpr = do
  n <- many1 digit <* whitespace
  return . ConstExpr $ read n

operators =
  [ [op "-" NegExpr, op "!" NotExpr, op "~" BitWiseExpr]
  , [binOp "/" BinDivExpr, binOp "*" BinMultExpr]
  , [binOp "+" BinAddExpr, binOp "-" BinSubtExpr]
  , [binOp ">>" BinBitRightExpr, binOp "<<" BinBitLeftExpr]
  , [binOp ">" BinLogGTExpr, binOp ">=" BinLogGTThanExpr, binOp "<" BinLogLTExpr, binOp "<=" BinLogLTThanExpr]
  , [binOp "==" BinLogEqExpr, binOp "!=" BinLogNotEqExpr]
  , [binOp "&" BinBitAndExpr]
  , [binOp "^" BinBitXorExpr]
  , [binOp "|" BinBitOrExpr]
  , [binOp "&&" BinLogAndExpr]
  , [binOp "||" BinLogOrExpr]
  ]
 where
  binOp ch const = InfixL (binExpr ch const)
  op ch const = Prefix (unExpr ch const)

binExpr :: String -> BinExpr -> Parser (Expr -> Expr -> Expr)
binExpr ch expr = do
  void $ string ch <* whitespace
  return $ BinExpr expr
unExpr :: String -> UnExpr -> Parser (Expr -> Expr)
unExpr ch expr = do
  void $ string ch <* whitespace
  return $ UnExpr expr

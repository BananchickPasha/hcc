module Parser.CExpr where
import           AST
import           Parser.Lexemes

import           Text.ParserCombinators.Parsec as P
import           Text.ParserCombinators.Parsec.Expr

import           Data.Functor

parseManyExpr :: Parser [Expr]
parseManyExpr = many1 parseAnyExpr

parseAnyExpr :: Parser Expr
parseAnyExpr = opers $ inScope '(' parseAnyExpr <|> constVar <|> opers constVar
  where opers = buildExpressionParser operators

constVar = constIntExpr <|> varExpr
constIntExpr :: Parser Expr
constIntExpr = do
  n <- many1 digit <* whitespace
  return . ConstExpr $ read n

varExpr :: Parser Expr
varExpr = VarExpr <$> var

operators :: OperatorTable Char () Expr
operators =
  [ [op "-" NegExpr, op "!" NotExpr, op "~" BitWiseExpr]
  , [binOp "/" BinDivExpr, binOp "*" BinMultExpr]
  , [binOp "+" BinAddExpr, binOp "-" BinSubtExpr]
  , [binOp ">>" BinBitRightExpr, binOp "<<" BinBitLeftExpr]
  , [binOp ">" BinLogGTExpr, binOp ">=" BinLogGTEqExpr, binOp "<" BinLogLTExpr, binOp "<=" BinLogLTEqExpr]
  , [binOp "==" BinLogEqExpr, binOp "!=" BinLogNotEqExpr]
  , [binOp "^" BinBitXorExpr]
  , [binOp "|" BinBitOrExpr]
  , [binOp "&" BinBitAndExpr]
  , [binOp "&&" BinLogAndExpr]
  , [binOp "||" BinLogOrExpr]
  ]
 where
  binOp ch expr = Infix (common ch $ BinExpr expr) AssocLeft
  op ch expr = Prefix . common ch $ UnExpr expr
  common ch func = try $ whitespace *> string ch >> whitespace *> lookAhead parseAnyExpr $> func

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

constVar = choice $ map try $ reverse [constIntExpr, varExpr , funCallExpr]
constIntExpr :: Parser Expr
constIntExpr = do
  n <- many1 digit <* whitespace
  return . ConstExpr $ read n
funCallExpr :: Parser Expr
funCallExpr = do
  funName <- var
  params <- inScope '(' $ parseAnyExpr `sepBy` (char ',' <* whitespace)
  return $ FunCall funName params


varExpr :: Parser Expr
varExpr = VarExpr <$> var

operators :: OperatorTable Char () Expr
operators =
  [ [ Prefix . common "++" $ assignUn IncB, Postfix . common' "++" $ assignUn IncF
    , Prefix . common "--" $ assignUn DecB, Postfix . common' "--" $ assignUn DecF
    ]
  , [op "-" NegExpr, op "!" NotExpr, op "~" BitWiseExpr]
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
  , [ Infix (common "=" assExpr) AssocRight
    , binAss "+=" BinAddExpr, binAss "-=" BinSubtExpr, binAss "*=" BinMultExpr, binAss "/=" BinDivExpr
    , binAss ">>=" BinBitLeftExpr, binAss "<<=" BinBitRightExpr
    , binAss "&=" BinBitAndExpr, binAss "^=" BinBitXorExpr, binAss "|=" BinBitOrExpr 
    ]
  ]
 where
  binOp ch expr = Infix (common ch $ BinExpr expr) AssocLeft
  op ch expr = Prefix . common ch $ UnExpr expr
  common ch func = try $ whitespace *> string ch >> whitespace *> lookAhead parseAnyExpr $> func
  common' ch func = try $ whitespace *> string ch >> whitespace $> func
  assignFunc binExpr (VarExpr name) expr = Assign name $ AssignExpr (BinExpr binExpr (VarExpr name) expr)
  assignUn assType (VarExpr name) = Assign name assType 
  binAss ch constr = Infix (common ch $ assignFunc constr) AssocRight
  assExpr (VarExpr name) expr = Assign name $ AssignExpr expr

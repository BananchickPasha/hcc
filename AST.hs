module AST
  ( Statement(..)
  , Expr(..)
  , UnExpr(..)
  , BinExpr(..)
  , Var
  )
where

data Statement =
               Return Expr
               | Assign Var Expr
               | Function String [Statement] --right now only () functiosn
               deriving (Show)
data Expr = ConstExpr Int
          | UnExpr UnExpr Expr
          | BinExpr BinExpr Expr Expr
          deriving (Show)

data UnExpr = NegExpr
            | BitWiseExpr
            | NotExpr
            deriving (Show)

data BinExpr = BinAddExpr
             | BinSubtExpr
             | BinMultExpr
             | BinDivExpr

             | BinLogAndExpr
             | BinLogOrExpr
             | BinLogEqExpr
             | BinLogNotEqExpr
             | BinLogLTExpr
             | BinLogLTThanExpr
             | BinLogGTExpr
             | BinLogGTThanExpr

             | BinModulExpr

             | BinBitAndExpr
             | BinBitOrExpr
             | BinBitXorExpr

             | BinBitLeftExpr
             | BinBitRightExpr
             deriving (Show)
type Var = String

module AST
  ( Statement(..)
  , Expr(..)
  , UnExpr(..)
  , BinExpr(..)
  )
where

data Statement = Return (Maybe Expr)
               | Assign String (Maybe Expr)
               | Function String [Statement] --right now only () functiosn
               | If Expr [Statement] (Maybe Statement)
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
             | BinLogLTEqExpr
             | BinLogGTExpr
             | BinLogGTEqExpr

             | BinModulExpr

             | BinBitAndExpr
             | BinBitOrExpr
             | BinBitXorExpr

             | BinBitLeftExpr
             | BinBitRightExpr
             deriving (Show)

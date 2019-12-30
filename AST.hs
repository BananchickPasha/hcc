module AST
  ( Statement(..)
  , Expr(..)
  , UnExpr(..)
  , BinExpr(..)
  )
where

data Statement = Return (Maybe Expr)
               | Expr Expr
               | Assign String Expr
               | Block [Statement]
               | Decl   String (Maybe Expr)
               | Function String [Statement] --right now only () functiosn
               | If Expr Statement (Maybe Statement)
               | WhileLoop Expr Statement
               deriving (Show)

data Expr = ConstExpr Int
          | VarExpr String
          | UnExpr UnExpr Expr
          | BinExpr BinExpr Expr Expr
          deriving (Show)

data UnExpr = NegExpr
            | BitWiseExpr
            | NotExpr
            | IncPostfix
            | IncPrefix
            | DecPrefix
            | DecPostfix
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

module AST
  ( Statement(..)
  , Expr(..)
  , UnExpr(..)
  , BinExpr(..)
  , AssignType(..)
  )
where

data Statement = Return (Maybe Expr)
               | Expr Expr
               | Block [Statement]
               | Empty
               | Decl   String (Maybe Expr)
               | Function String [String] Statement --right now only () functiosn
               | If Expr Statement (Maybe Statement)
               | WhileLoop Expr Statement
               | Break
               | Continue
               deriving (Show)

data Expr = ConstExpr Int
          | VarExpr String
          | UnExpr UnExpr Expr
          | Assign String AssignType
          | BinExpr BinExpr Expr Expr
          | FunCall String [Expr]
          deriving (Show)

data AssignType = AssignExpr Expr
                | IncF | IncB | DecF | DecB
                  {--| AssignAdd Expr | AssignSub Expr | AssignDiv Expr | AssignMult Expr
                | AssignAnd Expr | AssignOr Expr | AssignXor Expr
                | AssignLeft Expr | AssignRight Expr --}
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

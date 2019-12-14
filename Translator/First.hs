module Translator.First where
import AST
import Control.Applicative

trStatement :: Statement -> String
trStatement (Function name body) =
  unlines $ (".globl " ++ name) : (name++":") : map trStatement body

trStatement (Return expr) =
  unlines [trExpr expr , "ret"]


trExpr :: Expr -> String
trExpr (ConstExpr x) = "mov $" ++ show x ++ ", %rax"
trExpr (UnExpr expr x) = trExpr' expr
  where
  trExpr' NegExpr = unlines [trExpr x, "neg %rax"]
  trExpr' BitWiseExpr = unlines [trExpr x, "not %rax"]
  trExpr' NotExpr = unlines [trExpr x, "cmp $0, %rax", "mov $0, %rax", "sete %al"]
trExpr (BinExpr expr a b) = trExpr' expr
  where 
    -- rbx is a 
    -- rax is b
  trExpr' BinAddExpr = trStack ["add %rbx, %rax"]
  trExpr' BinSubtExpr = trStack ["sub %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinMultExpr = trStack ["imul %rbx, %rax"]
  trExpr' BinDivExpr = trStack' ["cqo", "idiv %rbx"] b a
  trStack instructions = trStack' instructions a b
  trStack' instructions a' b' = unlines $ [trExpr a', "push %rax", trExpr b', "pop %rbx"] ++ instructions


{-# LANGUAGE QuasiQuotes #-}
module Translator.First (trAST) where
import AST
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Sequence.Internal
import Data.Functor
import qualified Data.Map as M
import Distribution.Compat.Parsing (choice)
import Translator.State

import PyF



trAST :: [Statement] -> String
trAST stms = let initState = TranslatorST {variables = [M.empty], stackIndex = 0, errors = [], warnings = [], labels = M.empty} 
                 func = runState (trStatements stms) initState
                 (state, code) = func
              in case errors state of
                   [] -> code
                   xs -> concat $ Prelude.reverse xs

trStatements :: [Statement] -> State TranslatorST String
trStatements stms = concat <$> mapM trStatement stms
trStatement :: Statement -> State TranslatorST String
trStatement (Function name body) = do
  blocks <- mapM trStatement body
  return $ unlines $ [fmt|.globl {name}|] : [fmt|{name}:|] : "mov %rsp, %rbp" : blocks

trStatement (Return mExpr) = case mExpr of
                               Just expr -> trExpr expr >>= \expr' -> return $ unlines [expr', preRetCode, "ret"]
                               Nothing -> return $ unlines ["mov $0, %rax", preRetCode ,"ret"]
                             where preRetCode = "mov %rbp, %rsp"
trStatement (If expr stms els) = do
  blocks <- trStatement stms
  elseCode <- case els of 
                Nothing -> return ""
                Just e -> trStatement $ Block [e]
  expr' <- trExpr expr
  notThis <- incLabel "_notThis"
  return . unlines $ [expr' , "cmp $0, %rax", [fmt|je {notThis}|] , blocks, [fmt|{notThis}:|], elseCode] 
trStatement (Decl varName mval) = do
  val' <- getVarInFirst varName
  let val = fromMaybe (ConstExpr 0) mval
  case val' of 
    Just _ -> throwError "This var already exists" $> ""
    Nothing -> do 
      addVar varName
      valCode <- trExpr val
      return . unlines $ valCode : ["push %rax"]
trStatement (Assign varName val) = do
  val' <- getVar varName
  case val' of 
    Nothing -> throwError "This var does not exist" $> ""
    Just offset -> do 
      valCode <- trExpr val
      return . unlines $ valCode : [[fmt|mov %rax, {offset}(%rbp)|]]
trStatement (Block stms) = do
  diveIn
  code <- trStatements stms
  outCode <- diveOut
  return $ code ++ outCode
trStatement (WhileLoop expr stm) = do
  expr <- trExpr expr
  stms <- trStatement stm
  loopBeg <- incLabel "_whileLoopBEGIN"
  loopEnd <- incLabel "_whileLoopEND"
  return $ unlines [loopBeg ++ ":", expr, "cmp $0, %rax", [fmt|je {loopEnd}|], stms, [fmt|jmp {loopBeg}|], loopEnd ++ ":"]

trExpr :: Expr -> State TranslatorST String
trExpr (ConstExpr x) = return [fmt|mov ${x}, %rax|]
trExpr (VarExpr varName) = do
  mOffset <- getVar varName
  case mOffset of
    Nothing -> throwError "This var does not exist" $> ""
    Just offset -> return [fmt|mov {offset}(%rbp), %rax|]

trExpr (UnExpr expr x) = trExpr x >>= \x' -> return $ trExpr' expr x'
  where
  trExpr' NegExpr x = unlines [x, "neg %rax"]
  trExpr' BitWiseExpr x = unlines [x, "not %rax"]
  trExpr' NotExpr x = unlines [x, "cmp $0, %rax", "mov $0, %rax", "sete %al"]
trExpr (BinExpr expr a b) = trExpr' expr
  where 
    -- rbx is a 
    -- rax is b
  trExpr' BinAddExpr = trStack ["add %rbx, %rax"]
  trExpr' BinSubtExpr = trStack ["sub %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinMultExpr = trStack ["imul %rbx, %rax"]
  trExpr' BinDivExpr = trStack' ["cqo", "idiv %rbx"] b a

  trExpr' BinBitRightExpr = trStack' ["mov %rbx, %rcx", "shr %cl, %rax"] b a
  trExpr' BinBitLeftExpr = trStack' ["mov %rbx, %rcx", "shl %cl, %rax"] b a
  trExpr' BinBitAndExpr = trStack ["and %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinBitOrExpr = trStack ["or %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinBitXorExpr = trStack ["xor %rax, %rbx", "mov %rbx, %rax"]

  trExpr' BinLogEqExpr = trStack ["cmp %rax, %rbx", "mov $0, %rax", "sete %al"]
  trExpr' BinLogNotEqExpr = trStack ["cmp %rax, %rbx", "mov $0, %rax", "setne %al"]

  trExpr' BinLogGTEqExpr = trStack ["cmp %rax, %rbx", "mov $0, %rax", "setge %al"]
  trExpr' BinLogLTEqExpr = trStack ["cmp %rax, %rbx", "mov $0, %rax", "setle %al"]
  trExpr' BinLogGTExpr = trExpr $ UnExpr NotExpr (BinExpr BinLogLTEqExpr a b)
  trExpr' BinLogLTExpr = trExpr $ UnExpr NotExpr (BinExpr BinLogGTEqExpr a b)


  trExpr' BinLogOrExpr = do
    parsedA <- trExpr a
    parsedB <- trExpr b
    label <- incLabel "_lazyOR"
    return $ unlines [parsedA, "cmp $1, %rax", [fmt|je {label}|], parsedB, label ++ ":"]
  trExpr' BinLogAndExpr = do
    parsedA <- trExpr a
    parsedB <- trExpr b
    label <- incLabel "_lazyAND"
    return $ unlines [parsedA, "cmp $0, %rax", [fmt|je {label}|], parsedB, label ++ ":"]
  trStack instructions = trStack' instructions a b
  trStack' :: [String] -> Expr -> Expr -> State TranslatorST String
  trStack' instructions a' b' = do
    parsedA <- trExpr a'
    parsedB <- trExpr b'
    return . unlines $ [parsedA, "push %rax", parsedB, "pop %rbx"] ++ instructions


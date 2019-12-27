module Translator.First (trAST) where
import AST
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Sequence.Internal
import Data.Functor
import qualified Data.Map as M
import Distribution.Compat.Parsing (choice)
import Translator.State



trAST :: [Statement] -> String
trAST stms = let initState = TranslatorST {variables = [M.empty], stackIndex = 0, errors = [], warnings = []} 
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
  return $ unlines $ (".globl " ++ name) : (name++":") : "mov %rsp, %rbp" : blocks

trStatement (Return mExpr) = case mExpr of
                               Just expr -> trExpr expr >>= \expr' -> return $ unlines [expr', preRetCode, "ret"]
                               Nothing -> return $ unlines ["mov $0, %rax", preRetCode ,"ret"]
                             where preRetCode = "mov %rbp, %rsp"
trStatement (If expr stms els) = do
  blocks <- mapM trStatement stms
  elseCode <- case els of 
                Nothing -> return ""
                Just e -> trStatement e
  expr' <- trExpr expr
  return . unlines $ [expr' , "cmp $0, %rax", "je _notThis"] ++ blocks ++ ["_notThis:", elseCode] 
trStatement (Decl varName mval) = do
  val' <- getVar varName
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
      return . unlines $ valCode : ["mov %rax, " ++ show offset ++ "(%rbp)"]
trStatement (Block stms) = do
  diveIn
  code <- trStatements stms
  diveOut
  return code

trExpr :: Expr -> State TranslatorST String
trExpr (ConstExpr x) = return $ "mov $" ++ show x ++ ", %rax"
trExpr (VarExpr varName) = do
  mOffset <- getVar varName
  case mOffset of
    Nothing -> throwError "This var does not exist" $> ""
    Just offset -> return $ "mov " ++ show offset ++ "(%rbp)" ++ ", %rax"

trExpr (UnExpr expr x) = trExpr x >>= \x' -> return $ trExpr' expr x'
  where
  trExpr' NegExpr x = unlines [x, "neg %rax"]
  trExpr' BitWiseExpr x = unlines [x, "not %rax"]
  trExpr' NotExpr x = unlines [x, "cmp $0, %rax", "mov $0, %rax", "sete %al"]
trExpr e@(BinExpr expr a b) = trExpr' expr
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


  trExpr' BinLogOrExpr = parseTwo a b $ \a b -> return $ unlines [a, "cmp $1, %rax", "je _lazyOR", b, "_lazyOR:"]
  trExpr' BinLogAndExpr = parseTwo a b $ \a b -> return $ unlines [a, "cmp $0, %rax", "je _lazyAND", b, "_lazyAND:"]
  trStack instructions = trStack' instructions a b
  trStack' :: [String] -> Expr -> Expr -> State TranslatorST String
  trStack' instructions a' b' = parseTwo a' b' $ \parsedA parsedB -> return . unlines $ [parsedA, "push %rax", parsedB, "pop %rbx"] ++ instructions
  parseTwo :: Expr -> Expr -> (String -> String -> State TranslatorST String) -> State TranslatorST String
  parseTwo a b f = do
    parsedA <- trExpr a
    parsedB <- trExpr b
    f parsedA parsedB



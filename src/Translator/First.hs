{-# LANGUAGE QuasiQuotes #-}
module Translator.First
  ( trAST
  )
where
import           AST
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence.Internal         ( State(..) )
import           Data.Functor
import           Translator.State

import           PyF



trAST :: [Statement] -> String
trAST stms = let
      func          = runState (trStatements stms) initState
      (state, code) = func
  in  case _errors state of
        [] -> code
        xs -> concat $ Prelude.reverse xs

trStatements :: [Statement] -> State TranslatorST String
trStatements stms = concat <$> mapM trStatement stms
trStatement :: Statement -> State TranslatorST String
trStatement AST.Empty                = return ""
trStatement (Expr expr             ) = trExpr expr
trStatement (Function "main" _ body) = do
  blocks <- trStatement body --TODo divein but for funcs
  return
    $ unlines
    $ ".globl main"
    : "main:"
    : "push %rbp"
    : "mov %rsp, %rbp"
    : [blocks]
trStatement (Function name params body) = do
  diveInFunc
  addFunc name $ length params
  incIndex 8
  mapM_ (`addVar` (-8)) params
  setIndex 0
  --incIndex $ length params * 8
  blocks <- trStatement body --TODo divein but for funcs
  diveOutFunc
  return
    $ unlines
    $ [fmt|.globl {name}|]
    : [fmt|{name}:|]
    : "push %rbp"
    : "mov %rsp, %rbp"
    -- : [fmt|add ${(length params + 2) * 8}, %rbp|]
    : [blocks]

trStatement (Return mExpr) = do
  let preRetCode = ["mov %rbp, %rsp", "pop %rbp", "ret"]
  case mExpr of
    Just expr ->
      trExpr expr >>= \expr' -> return $ unlines $ expr' : preRetCode
    Nothing -> return $ unlines $ "mov $0, %rax" : preRetCode
trStatement (If expr stms els) = do
  blocks   <- trStatement stms
  elseCode <- case els of
    Nothing -> return ""
    Just e  -> trStatement $ Block [e]
  expr'   <- trExpr expr
  notThis <- incLabel "_notThis"
  return
    . unlines
    $ [ expr'
      , "cmp $0, %rax"
      , [fmt|je {notThis}|]
      , blocks
      , [fmt|{notThis}:|]
      , elseCode
      ]
trStatement (Decl varName mval) = do
  val' <- getVarInFirst varName
  let val = fromMaybe (ConstExpr 0) mval
  case val' of
    Just _  -> throwError "This var already exists" $> ""
    Nothing -> do
      addInt varName
      valCode <- trExpr val
      return . unlines $ valCode : ["push %rax"]
trStatement (Block stms) = do
  diveInLocal
  code    <- trStatements stms
  outCode <- diveOutLocal
  return $ unlines [code, outCode]
trStatement (WhileLoop expr stm) = do
  expr    <- trExpr expr
  stms    <- trStatement stm
  loopBeg <- incLabel "_whileLoopBEGIN"
  loopEnd <- incLabel "_whileLoopEND"
  return $ unlines
    [ loopBeg ++ ":"
    , expr
    , "cmp $0, %rax"
    , [fmt|je {loopEnd}|]
    , stms
    , [fmt|jmp {loopBeg}|]
    , loopEnd ++ ":"
    ]
trStatement Break = do
  label <- getLabel "_whileLoopEND"
  return $ "jmp " ++ label
trStatement Continue = do
  label <- getLabel "_whileLoopBEGIN"
  return $ "jmp " ++ label
trExpr :: Expr -> State TranslatorST String
trExpr (ConstExpr x) = return [fmt|mov ${x}, %rax|]

trExpr (VarExpr varName) =
  tryGetVar varName $ \offset -> return [fmt|mov {offset}(%rbp), %rax|]
trExpr (FunCall funName params) = do
  func <- getFunc funName
  case func of
    Nothing  -> throwError "This func does not exist" $> ""
    Just val -> do
      pushes' <- mapM trExpr $ reverse params
      if Prelude.length pushes' /= val
        then
          throwError
              [fmt|Not enough params excepted: {Prelude.length pushes'} got: {val} |]
            $> ""
        else do
          let pushes = map (++ "\npush %rax") pushes'
          return $ unlines $ pushes ++ [[fmt|call {funName}|], [fmt|add ${length pushes * 8}, %rsp|]]
trExpr (Assign varName (AssignExpr val)) = tryGetVar varName $ \offset -> do
  valCode <- trExpr val
  return . unlines $ valCode : [[fmt|mov %rax, {offset}(%rbp)|]]
trExpr (Assign varName IncF) = tryGetVar varName $ \offset -> return $ unlines
  [ [fmt|mov {offset}(%rbp), %rax|]
  , "push %rax"
  , "inc %rax"
  , [fmt|mov %rax, {offset}(%rbp)|]
  , "pop %rax"
  ]
trExpr (Assign varName DecF) = tryGetVar varName $ \offset -> return $ unlines
  [ [fmt|mov {offset}(%rbp), %rax|]
  , "push %rax"
  , "dec %rax"
  , [fmt|mov %rax, {offset}(%rbp)|]
  , "pop %rax"
  ]
trExpr (Assign varName DecB) = tryGetVar varName $ \offset -> return $ unlines
  [[fmt|mov {offset}(%rbp), %rax|], "dec %rax", [fmt|mov %rax, {offset}(%rbp)|]]
trExpr (Assign varName IncB) = tryGetVar varName $ \offset -> return $ unlines
  [[fmt|mov {offset}(%rbp), %rax|], "inc %rax", [fmt|mov %rax, {offset}(%rbp)|]]
--trExpr (Assign varName Inc) = tryGetVar varName $ \offset -> return $ unlines [[fmt|mov {offset}(%rbp), %rax|], "inc %rax", [fmt|mov %rax, {offset}(%rbp)|]]
trExpr (UnExpr expr x) = trExpr x >>= \x' -> return $ trExpr' expr x'
 where
  trExpr' NegExpr x = unlines [x, "neg %rax"]
  trExpr' BitWiseExpr x = unlines [x, "not %rax"]
  trExpr' NotExpr x = unlines [x, "cmp $0, %rax", "mov $0, %rax", "sete %al"]
trExpr (BinExpr expr a b) = trExpr' expr
 where
    -- rbx is a 
    -- rax is b
  trExpr' BinAddExpr      = trStack ["add %rbx, %rax"]
  trExpr' BinSubtExpr     = trStack ["sub %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinMultExpr     = trStack ["imul %rbx, %rax"]
  trExpr' BinDivExpr      = trStack' ["cqo", "idiv %rbx"] b a

  trExpr' BinBitRightExpr = trStack' ["mov %rbx, %rcx", "shr %cl, %rax"] b a
  trExpr' BinBitLeftExpr  = trStack' ["mov %rbx, %rcx", "shl %cl, %rax"] b a
  trExpr' BinBitAndExpr   = trStack ["and %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinBitOrExpr    = trStack ["or %rax, %rbx", "mov %rbx, %rax"]
  trExpr' BinBitXorExpr   = trStack ["xor %rax, %rbx", "mov %rbx, %rax"]

  trExpr' BinLogEqExpr = trStack ["cmp %rax, %rbx", "mov $0, %rax", "sete %al"]
  trExpr' BinLogNotEqExpr =
    trStack ["cmp %rax, %rbx", "mov $0, %rax", "setne %al"]

  trExpr' BinLogGTEqExpr =
    trStack ["cmp %rax, %rbx", "mov $0, %rax", "setge %al"]
  trExpr' BinLogLTEqExpr =
    trStack ["cmp %rax, %rbx", "mov $0, %rax", "setle %al"]
  trExpr' BinLogGTExpr = trExpr $ UnExpr NotExpr (BinExpr BinLogLTEqExpr a b)
  trExpr' BinLogLTExpr = trExpr $ UnExpr NotExpr (BinExpr BinLogGTEqExpr a b)


  trExpr' BinLogOrExpr = do
    parsedA <- trExpr a
    parsedB <- trExpr b
    label   <- incLabel "_lazyOR"
    return $ unlines
      [parsedA, "cmp $1, %rax", [fmt|je {label}|], parsedB, label ++ ":"]
  trExpr' BinLogAndExpr = do
    parsedA <- trExpr a
    parsedB <- trExpr b
    label   <- incLabel "_lazyAND"
    return $ unlines
      [parsedA, "cmp $0, %rax", [fmt|je {label}|], parsedB, label ++ ":"]
  trStack instructions = trStack' instructions a b
  trStack' :: [String] -> Expr -> Expr -> State TranslatorST String
  trStack' instructions a' b' = do
    parsedA <- trExpr a'
    parsedB <- trExpr b'
    return
      .  unlines
      $  [parsedA, "push %rax", parsedB, "pop %rbx"]
      ++ instructions


tryGetVar
  :: String -> (Int -> State TranslatorST String) -> State TranslatorST String
tryGetVar varName f = do
  mOffset <- getVar varName
  case mOffset of
    Nothing     -> throwError "This var does not exist" $> ""
    Just offset -> f offset

module Translator.First where
import AST
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Sequence.Internal
import qualified Data.Map as M

data TranslatorST = TranslatorST { variables :: M.Map String (Maybe Int)
                                 , stackIndex :: Int
                                 , resCode :: String
                                 }

addVar :: String -> Maybe Int -> State TranslatorST ()
addVar varName mVal = State $ \xs -> let newVars = M.insert varName mVal (variables xs)
                                         newStack = stackIndex xs - 1
                                      in (xs {variables = newVars, stackIndex = newStack}, ())

getVar :: String -> State TranslatorST (Maybe Int)
getVar varName = State $ \xs -> (xs, fromMaybe Nothing (variables xs M.!? varName))
getCode :: State TranslatorST String
getCode = State $ \xs -> (xs, resCode xs)
changeCode :: String -> State TranslatorST String
changeCode code = State $ \xs -> (xs {resCode = code}, code)


trStatement :: Statement -> State TranslatorST String
trStatement (Function name body) = do
  blocks <- mapM trStatement body
  changeCode $ unlines $ (".globl " ++ name) : (name++":") : blocks

trStatement (Return (Just expr)) =
  unlines [trExpr expr , "ret"]
trStatement (If expr stms els) =
  unlines $ [trExpr expr , "cmp $0, %rax", "je _notThis"] ++ map trStatement stms 
  ++ ["_notThis:"] ++ case els of 
                   Nothing -> []
                   Just e -> [trStatement e]

trExpr :: Expr -> String
trExpr (ConstExpr x) = "mov $" ++ show x ++ ", %rax"
trExpr (UnExpr expr x) = trExpr' expr
  where
  trExpr' NegExpr = unlines [trExpr x, "neg %rax"]
  trExpr' BitWiseExpr = unlines [trExpr x, "not %rax"]
  trExpr' NotExpr = unlines [trExpr x, "cmp $0, %rax", "mov $0, %rax", "sete %al"]
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


  trExpr' BinLogOrExpr = unlines [trExpr a, "cmp $1, %rax", "je _lazyOR", trExpr b, "_lazyOR:"]
  trExpr' BinLogAndExpr = unlines [trExpr a, "cmp $0, %rax", "je _lazyAND", trExpr b, "_lazyAND:"]
  trStack instructions = trStack' instructions a b
  trStack' instructions a' b' = unlines $ [trExpr a', "push %rax", trExpr b', "pop %rbx"] ++ instructions


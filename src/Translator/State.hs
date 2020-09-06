{-# LANGUAGE QuasiQuotes #-}
module Translator.State where
import           Data.Sequence.Internal
import qualified Data.Map                      as M
import           Distribution.Compat.Parsing    ( choice )
import           Data.Maybe (fromMaybe)
import PyF


data TranslatorST = TranslatorST { variables :: [[M.Map String Int]] --frames per func with frames per visibility in it
                                 , stackIndex :: [Int] -- frames per func
                                 , labels :: M.Map String Int
                                 , errors :: [String]
                                 , warnings :: [String]
                                 , funcs :: M.Map String Int
                                 }

data Var = Int
         | Func Int

addInt :: String -> State TranslatorST ()
addInt varName = addVar varName 8
addVar :: String -> Int -> State TranslatorST ()
addVar varName size = State $ \xs ->
  let currentIndex = head $ stackIndex xs
      currentVars  = head $ variables xs
      newStack = currentIndex - size
      newVars  = M.insert varName newStack (head currentVars)
  in  ( xs { variables  = changeFirst (changeFirst newVars currentVars) (variables xs)
           , stackIndex = changeFirst newStack $ stackIndex xs
           }, ())
addFunc :: String -> Int -> State TranslatorST ()
addFunc funName paramCount = State $ \xs -> (xs {funcs = M.insert funName paramCount (funcs xs)}, ())
getFunc :: String -> State TranslatorST (Maybe Int)
getFunc funName = State $ \xs -> (xs, funcs xs M.!? funName)
incIndex inc = State $ \xs -> (xs {stackIndex = changeFirst (head (stackIndex xs) + inc) $ stackIndex xs}, ())
setIndex inc = State $ \xs -> (xs {stackIndex = changeFirst inc $ stackIndex xs}, ())

getVar :: String -> State TranslatorST (Maybe Int)
getVar varName = State $ \xs -> (xs, findFitst varName (head $ variables xs))
getVarInFirst :: String -> State TranslatorST (Maybe Int)
getVarInFirst varName = State $ \xs -> (xs, head (head $ variables xs) M.!? varName)
throwError :: String -> State TranslatorST ()
throwError err = State $ \xs -> (xs { errors = err : errors xs }, ())


diveInLocal :: State TranslatorST ()
diveInLocal = State $ \xs -> (xs { variables = changeFirst (M.empty : head (variables xs)) (variables xs) }, ())
diveOutLocal :: State TranslatorST String
diveOutLocal = State $ \xs -> 
  let offSize = 8 * M.size (head . head $ variables xs)
      newIndex = (head (stackIndex xs) + offSize) 
   in
  ( xs { variables  = changeFirst (tail . head $ variables xs) $ variables xs
       , stackIndex = changeFirst newIndex $ stackIndex xs
       }, [fmt|add ${offSize}, %rsp|])

diveInFunc :: State TranslatorST ()
diveInFunc = State $ \xs -> (xs { variables = [M.empty] : variables xs, stackIndex = 0 : stackIndex xs }, ())
diveOutFunc = State $ \xs -> (xs { variables = tail $ variables xs, stackIndex = tail $ stackIndex xs }, ())
getStack = State $ \xs -> (xs , head $ stackIndex xs)


incLabel :: String -> State TranslatorST String
incLabel label = State $ \xs -> let (mVal, newMap) = M.insertLookupWithKey f label 1 (labels xs) 
                                    f _ = (+)
                                    newLabel = label ++ show (1 + fromMaybe 0 mVal)
                                 in (xs {labels = newMap }, newLabel)
getLabel :: String -> State TranslatorST String
getLabel label = State $ \xs -> let mVal = labels xs M.!? label 
                                 in (xs, label ++ show (fromMaybe 1 mVal))

findFitst :: Ord k => k -> [M.Map k v] -> Maybe v
findFitst key maps = choice $ map (M.!? key) maps
changeFirst :: a -> [a] -> [a]
changeFirst new (_ : xs) = new : xs

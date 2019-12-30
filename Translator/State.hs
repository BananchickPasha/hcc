{-# LANGUAGE QuasiQuotes #-}
module Translator.State where
import           Data.Sequence.Internal
import qualified Data.Map                      as M
import           Distribution.Compat.Parsing    ( choice )
import           Data.Maybe (fromMaybe)
import PyF


data TranslatorST = TranslatorST { variables :: [M.Map String Int]
                                 , stackIndex :: Int
                                 , labels :: M.Map String Int
                                 , errors :: [String]
                                 , warnings :: [String]
                                 }

addVar :: String -> State TranslatorST ()
addVar varName = State $ \xs ->
  let newStack = stackIndex xs - 8
      newVars  = M.insert varName newStack (head $ variables xs)
  in  ( xs { variables  = changeFirst newVars (variables xs)
           , stackIndex = newStack
           }, ())

getVar :: String -> State TranslatorST (Maybe Int)
getVar varName = State $ \xs -> (xs, findFitst varName (variables xs))
getVarInFirst :: String -> State TranslatorST (Maybe Int)
getVarInFirst varName = State $ \xs -> (xs, head (variables xs) M.!? varName)
throwError :: String -> State TranslatorST ()
throwError err = State $ \xs -> (xs { errors = err : errors xs }, ())


diveIn :: State TranslatorST ()
diveIn = State $ \xs -> (xs { variables = M.empty : variables xs }, ())
diveOut :: State TranslatorST String
diveOut = State $ \xs -> 
  let offSize = 8 * M.size (head $ variables xs)
      newIndex = (stackIndex xs + offSize) 
   in
  ( xs { variables  = tail $ variables xs
       , stackIndex = newIndex
       }, [fmt|add ${offSize}, %rsp|])

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

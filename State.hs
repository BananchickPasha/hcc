module Translator.State where
import           Data.Sequence.Internal
import qualified Data.Map                      as M
import           Distribution.Compat.Parsing    ( choice )


data TranslatorST = TranslatorST { variables :: [M.Map String Int]
                                 , stackIndex :: Int
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
throwError :: String -> State TranslatorST ()
throwError err = State $ \xs -> (xs { errors = err : errors xs }, ())


diveIn :: State TranslatorST ()
diveIn = State $ \xs -> (xs { variables = M.empty : variables xs }, ())
diveOut :: State TranslatorST ()
diveOut = State $ \xs ->
  ( xs { variables  = tail $ variables xs
       , stackIndex = stackIndex xs + 8 * M.size (head $ variables xs)
       }, ())



findFitst :: Ord k => k -> [M.Map k v] -> Maybe v
findFitst key maps = choice $ map (M.!? key) maps
changeFirst :: a -> [a] -> [a]
changeFirst new (_ : xs) = new : xs

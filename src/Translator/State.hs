{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Translator.State
  ( TranslatorST
  , initState
  , addInt
  , addVar
  , addFunc
  , getFunc
  , incIndex
  , setIndex
  , getVar
  , getVarInFirst
  , throwError
  , diveInLocal
  , diveOutLocal
  , diveInFunc
  , diveOutFunc
  , incLabel
  , getLabel
  , _errors
  )
where
import           Data.Sequence.Internal
import qualified Data.Map                      as M
import           Distribution.Compat.Parsing    ( choice )
import           Data.Maybe                     ( fromMaybe )

import           PyF
import           Lens.Micro.TH
import           Lens.Micro


data TranslatorST = TranslatorST { _variables :: [[M.Map String Int]] --frames per func with frames per visibility in it
                                 , _stackIndex :: [Int] -- frames per func
                                 , _labels :: M.Map String Int
                                 , _errors :: [String]
                                 , _funcs :: M.Map String Int
                                 }
$(makeLenses ''TranslatorST)
initState = TranslatorST { _variables  = [[M.empty]]
                         , _stackIndex = [0]
                         , _errors     = []
                         , _labels     = M.empty
                         , _funcs      = M.empty
                         }
toTuple :: a -> b -> (b, a)
toTuple a b = (b, a)
addInt :: String -> State TranslatorST ()
addInt varName = addVar varName 8
addVar :: String -> Int -> State TranslatorST ()
addVar varName size = State $ \xs ->
  let currentIndex = head $ xs ^. (stackIndex)
      newStack     = currentIndex - size
  in  toTuple ()
        $ over (variables . ix 0 . ix 0) (M.insert varName newStack)
        . set (stackIndex . ix 0) newStack
        $ xs

addFunc :: String -> Int -> State TranslatorST ()
addFunc funName paramCount =
  State $ \xs -> toTuple () $ over funcs (M.insert funName paramCount) $ xs
getFunc :: String -> State TranslatorST (Maybe Int)
getFunc funName = State $ \xs -> (xs, (xs ^. funcs) M.!? funName)
incIndex :: Int -> State TranslatorST ()
setIndex :: Int -> State TranslatorST ()
incIndex inc = State $ \xs -> toTuple () $ (stackIndex . ix 0) +~ inc $ xs
setIndex inc = State $ \xs -> toTuple () $ (stackIndex . ix 0) .~ inc $ xs

getVar :: String -> State TranslatorST (Maybe Int)
getVar varName = State
  $ \xs -> (xs, findFirst varName $ xs ^. (variables . ix 0))
  where findFirst key maps = choice $ map (M.!? key) maps
getVarInFirst :: String -> State TranslatorST (Maybe Int)
getVarInFirst varName =
  State $ \xs -> (xs, (xs ^. (variables . ix 0 . ix 0)) M.!? varName)
throwError :: String -> State TranslatorST ()
throwError err = State $ \xs -> toTuple () $ over errors (err :) xs


diveInLocal :: State TranslatorST ()
diveInLocal =
  State $ \xs -> toTuple () $ over (variables . ix 0) (M.empty :) xs
diveOutLocal :: State TranslatorST String
diveOutLocal = State $ \xs ->
  let offSize = 8 * M.size (head . head $ _variables xs)
  in  toTuple [fmt|add ${offSize}, %rsp|]
        $ ((stackIndex . ix 0) +~ offSize)
        . over (variables . ix 0) tail
        $ xs

diveInFunc :: State TranslatorST ()
diveInFunc = State $ \xs ->
  toTuple () $ over variables ([M.empty] :) . over stackIndex (0 :) $ xs
diveOutFunc :: State TranslatorST ()
diveOutFunc =
  State $ \xs -> toTuple () $ over variables tail . over stackIndex tail $ xs
getStack :: State TranslatorST Int
getStack = State $ \xs -> (xs, head $ _stackIndex xs)


incLabel :: String -> State TranslatorST String
incLabel label = State $ \xs ->
  let (mVal, newMap) = M.insertLookupWithKey f label 1 (xs ^. labels)
      f _ = (+)
      newLabel = label ++ show (1 + fromMaybe 0 mVal)
  in  toTuple newLabel $ labels .~ newMap $ xs
getLabel :: String -> State TranslatorST String
getLabel label = State $ \xs ->
  let mVal = _labels xs M.!? label in (xs, label ++ show (fromMaybe 1 mVal))


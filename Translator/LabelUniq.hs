module Translator.LabelUniq where

import qualified Data.Map                      as M
import           Data.List
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )

downToUp = ["_lazyOR", "_lazyAND", "_notThis"]

uniq :: [String] -> M.Map String Int -> [String]
uniq [] _ = []
uniq (str : xs) labels =
  let
    (newVal, newLabelMap) =
      M.updateLookupWithKey (\_ a -> Just $ 1 + a) (init str) labels
    lNames = words $ dropWhile ('_' /=) str
    lName = if not $ null lNames then head lNames else []
    showMaybe (Just val) = show val
    showMaybe Nothing = ""
  in
    if not (null str) && last str == ':'
      then (init str ++ showMaybe newVal ++ ":") : uniq xs newLabelMap
      else case M.lookup lName labels of
        Nothing -> str : uniq xs labels
        Just val ->
          T.unpack
              (T.replace (T.pack lName)
                         (T.pack $ lName ++ show val)
                         (T.pack str)
              )
            : uniq xs labels

makeitwork :: String -> String
makeitwork str = let labels = M.fromList (zip downToUp $ repeat 0)
                     reversed = reverse $ lines str
              in unlines . reverse $ uniq reversed labels


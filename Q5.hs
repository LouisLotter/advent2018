module Q5 where

import qualified Data.IntMap.Lazy as IntMap
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char

charEq :: Char -> Char -> Bool
charEq c c' = (toLower c) == (toLower c')

charNotEq :: Char -> Char -> Bool
charNotEq c c' = (toLower c) /= (toLower c')

react :: String -> String
react [] = []
react (s:s':str)
  | s == s'     = s : (react (s':str))
  | charEq s s' = react str
  | True        = s : (react (s':str))
react (s:str) = s:str

reduce :: String -> String
reduce str = reduce' str 0
  where
    reduce' :: String -> Int -> String
    reduce' str len
      | (length str) == len = str
      | True                = reduce' newStr (length str)
          where
            newStr = react str

permut :: String -> [String]
permut str = map (remove str) ['a'..'z']
  where
    remove str c = (filter (charNotEq c) str)


test = do
    f <- readFile "input5.txt"
    let
      reducedF = reduce f
    putStrLn $ show $ length f
    putStrLn $ show $ (length $ reducedF)
    putStrLn $ show $ sort $ map (length . reduce) $ permut reducedF



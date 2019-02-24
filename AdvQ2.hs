module AdvQ2 where

import Data.Maybe
import Text.Printf
import System.IO
import Text.Read
import Data.List
import Data.HashMap.Strict as DHS


----------------------part 1--------------------------------
toOccurrences ::  HashMap Char Int -> String -> HashMap Char Int
toOccurrences acc [] = acc
toOccurrences acc (c:str) = toOccurrences (insertWith (+) c 1 acc) str

containCharXTimes :: HashMap Char Int -> Int -> Bool
containCharXTimes hMap x = (size $ DHS.filter ((==) x) hMap) > 0

twoOrThree :: String -> (Bool,Bool)
twoOrThree str = (containCharXTimes occurrences 2 , containCharXTimes occurrences 3)
  where
    occurrences = toOccurrences DHS.empty str

twoOrThreeList :: [String] -> [(Bool,Bool)]
twoOrThreeList [] = []
twoOrThreeList (str:strs) = twoOrThree str : (twoOrThreeList strs)

calcCheckSum :: [(Bool,Bool)] -> Int
calcCheckSum = calcCheckSum' 0 0
  where
    calcCheckSum' acc acc' [] = acc * acc'
    calcCheckSum' acc acc' ((True,True):bs) = calcCheckSum' (acc+1) (acc'+1) bs
    calcCheckSum' acc acc' ((True,_):bs)    = calcCheckSum' acc (acc'+1) bs
    calcCheckSum' acc acc' ((_,True):bs)    = calcCheckSum' (acc+1) acc' bs
    calcCheckSum' acc acc' ((_,_):bs)       = calcCheckSum' acc acc' bs


---------------------part2

lengthDiff :: String -> Int -> String -> Bool
lengthDiff = lengthDiff' 0
  where
    lengthDiff' acc [] reqDiff [] = acc == reqDiff
    lengthDiff' acc (c:str) reqDiff (c':str')
      | acc > reqDiff = False
      | c == c'       = lengthDiff' acc str reqDiff str'
      | True          = lengthDiff' (acc+1) str reqDiff str'

isThisIt :: String -> [String] -> Int -> (Bool,String)
isThisIt str [] _ = (False,"")
isThisIt str (str':strs) reqDiff
  | lengthDiff str reqDiff str' = (True,str')
  | True                        = isThisIt str strs reqDiff

findBoxes :: [String] -> Int -> (String,String)
findBoxes [] reqDiff = ("","")
findBoxes (str:strs) reqDiff = case (isThisIt str strs reqDiff) of
   (True,str') -> (str,str')
   _ -> findBoxes strs reqDiff


test = do
    f     <- readFile "app/input2.txt"
    putStrLn $ show $ calcCheckSum $ twoOrThreeList $ lines f
    putStrLn $ show $ findBoxes (lines f) 1


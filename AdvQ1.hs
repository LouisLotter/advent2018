module AdvQ1 where

import Data.Maybe
import Text.Printf
import System.IO
import Text.Read
import Data.List
import Data.HashMap.Strict as DHS


dropPlus :: [Char] -> [Char]
dropPlus [] = []
dropPlus (x:xs)
  | x == '+'  = xs
  | True      = x:xs

createIntList :: String -> [Int]
createIntList str = catMaybes $ fmap (readMaybe . dropPlus) $ lines str

findFirstRepeated :: HashMap Int Bool -> [Int] -> Maybe Int
findFirstRepeated _ [] = Nothing
findFirstRepeated cum xx@(x:xs)
  | xs == []  = Nothing
  | member x cum = Just x
  | True      = findFirstRepeated (DHS.insert x True cum)  xs

sumInstructions :: String -> Int
sumInstructions [] = 0
sumInstructions str = Data.List.foldr (+) 0 $ createIntList str

test = do
    f     <- readFile "app/input.txt"
    putStrLn $ show (sumInstructions f)
    putStrLn $ show $ findFirstRepeated DHS.empty $ 0:(scanl1 (+) $ cycle $ createIntList f)


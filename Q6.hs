module Q5 where

import qualified Data.IntMap.Lazy as IntMap
import Data.List.Split
import Data.Maybe
import Data.List

tupDist :: (Int, Int) -> Int
tupDist (_, d) = d

genNumberedList :: [String] -> [(Int, Int, Int)]
genNumberedList strs = map toTuple strs
  where
    toTuple str = (x*1000+y,x, y)
      where
        (x, y) = (read x',read y')
          where
            (x':y':_) = splitOn ", " str

getActiveArea :: [(Int, Int, Int)] -> (Int, Int, Int ,Int)
getActiveArea coords = foldr areacalc (1000,0,1000,0) coords
  where
    areacalc :: (Int, Int, Int) -> (Int, Int, Int ,Int) -> (Int, Int, Int ,Int)
    areacalc (_, x, y) (xf, xt, yf, yt) = (xf', xt', yf', yt')
      where
        xf' = if (x < xf) then x else xf
        xt' = if (x > xt) then x else xt
        yf' = if (y < yf) then y else yf
        yt' = if (y > yt) then y else yt

genAA :: (Int, Int, Int ,Int) -> [(Int, Int, Int)]
genAA (xf, xt, yf, yt) = [(x*1000+y,x, y) | x <- [xf..xt], y <- [yf..yt]]

distanceTo::  [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int, [(Int, Int)])
distanceTo coords loc@(id, x, y) = (id, x, y, map (manhattan loc) coords)
  where
    manhattan (_ , x, y) (id', x', y') = (id', abs(x' - x) + abs(y' - y))


countAreas :: [(Int, Int, Int, [(Int, Int)])] -> IntMap.IntMap Int
countAreas nodes = IntMap.fromListWith (+) $ map (flat . getClosest) nodes
  where
    flat (_, _, _, (id, _)) = (id, 1)
    getClosest :: (Int, Int, Int, [(Int, Int)]) ->  (Int, Int, Int, (Int, Int))
    getClosest (id, x, y, dsts) = if (dA == dB) then (id, x, y, (-1, 0)) else (id, x, y,(idA, dA))
      where
        ((idA, dA):(idB, dB):_) = sortOn tupDist dsts

findNodesWithinXofAll :: Int -> [(Int, Int, Int, [(Int, Int)])] -> [(Int, Int, Int, [(Int, Int)])]
findNodesWithinXofAll x nodes = filter (withinxOffAll x) nodes
  where
    withinxOffAll x (_, _, _, ds) = (foldr (withinX x) 0 ds) < x
      where
        withinX x (_, d) total = d + total


test = do
    f <- readFile "input6.txt"
    let
      coords = genNumberedList $ lines f
      zone   = getActiveArea coords
      raw    = map (distanceTo coords) $ genAA zone
    putStrLn $ show $ sortOn tupDist $ IntMap.toList $ countAreas raw
    putStrLn $ show $ length $ findNodesWithinXofAll 10000 raw

module Q3 where


import qualified Data.Map.Lazy as Map
import Data.List.Split
import Data.List
import Data.Char
import Data.List.Unique


getBot :: String -> (Int,Int,Int,Int)
getBot str = case splitOn "," str of
  x:y:z:r:_ ->  (read x',read y,read z',read r')
    where
       _:x':_  = splitOn "<" x
       z':_    = splitOn ">" z
       _:r':_  = splitOn "=" r

getRange :: (Int,Int,Int,Int) -> Int
getRange (_,_,_,r) = r

manhattan :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int
manhattan (x, y, z, _) (x', y', z', _) = dst x x' + dst y y' + dst z z'
  where
    dst a b = abs (a - b)

inRange :: [(Int,Int,Int,Int)] -> Int
inRange (b:bs) = foldl' (addIfInRange b) 1 bs
  where
    addIfInRange b c b' = c + botInRange b b'
    botInRange b@(_, _, _, r) b' = if (r >= manhattan b b') then 1 else 0

---- part 2

doesIntersect :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
doesIntersect b@(_, _, _, r) b'@(_, _, _, r') = (r + r') >= manhattan b b'

intersectAnd :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool -> Bool
intersectAnd b b' cum = if not cum then False else doesIntersect b b'

intersectAll :: (Int, Int, Int, Int) -> [(Int,Int,Int,Int)] -> Bool
intersectAll b = foldr (intersectAnd b) True

bIntersect :: (Int, Int, Int, Int) -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
bIntersect bee bees = bIntersect' (bee:[]) bees
  where
    bIntersect' bs [] = bs
    bIntersect' bs (b':bs')
      | intersectAll b' bs  = let
                                a = bIntersect' (b':bs) bs'
                                b = bIntersect' bs bs'
                              in
--                                if length a >= length b then a else b
                                a
      | True                = bIntersect' bs bs'

findMaxIntersect :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
findMaxIntersect [] cum = cum
findMaxIntersect (b:bs) cum
 |  length potentialSet >= length cum = findMaxIntersect bs potentialSet
 |  True                              = findMaxIntersect bs cum
      where
        potentialSet = bIntersect b bs


test = do
    f <- readFile "input23.txt"
    let
      bots = fmap getBot $ lines f
      sortedBots = reverse $ sortOn getRange $ bots
      intersection = bIntersect (head sortedBots) (tail sortedBots)
    putStrLn $ show $ inRange sortedBots
    putStrLn $ show $ head sortedBots
    putStrLn $ show $ intersection
    putStrLn $ show $ length intersection
    --putStrLn $ show $ findMaxIntersect bots []





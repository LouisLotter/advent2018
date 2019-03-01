module Q3 where


import qualified Data.IntMap.Lazy as IntMap
import Data.List.Split
import Data.Maybe
import Data.List


genZone :: (String,Int,Int,Int,Int) -> (String,[(Int,Int)])
genZone (idd,l,t,w,h) = (idd,concat $ [genTup l w i | i <- [t..(t+h-1)]])
  where
    genTup l w offset = [(i,1) | i <- [key..(key+w-1)]]
      where
        key = l + (offset*1000)

getSegments :: String -> Maybe(String,Int,Int,Int,Int)
getSegments str = case splitOn " " str of
  idd:_:p:size:_ ->  Just (idd,read pa,read pb,read sa,read sb)
                        where
                           pa:pb:_  = splitOn "," (filter (/=':') p)
                           sa:sb:_  = splitOn "x" size
  _            ->  Nothing

tupMap f (a,b) = (a,(f b))
tupFlat (_, b) = b
tupSize (_, b) = b == 0

test = do
    f     <- readFile "input3.txt"
    let
      listOfMapClaims = map ((tupMap IntMap.fromAscList) . genZone) $ catMaybes $ map getSegments $ lines f
      overlapMap = IntMap.filter (> 1) $ IntMap.unionsWith (+) $ map tupFlat listOfMapClaims
    putStrLn $ show $ IntMap.size $ overlapMap
    putStrLn $ show $ filter tupSize $ map (tupMap $ IntMap.size . (IntMap.intersection overlapMap)) listOfMapClaims




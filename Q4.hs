module Q4 where

import qualified Data.IntMap.Lazy as IntMap
import Data.List.Split
import Data.Maybe
import Data.List

getSegments :: String -> Maybe(String, Int, Int, String, String)
getSegments str = case splitOn " " str of
  date:tim:event:id:_ -> Just ((filter (/='[') date),read h, read min, event, id)
                      where
                        h:min:_  = splitOn ":" (filter (/=']') tim)
  _                -> Nothing

dateOrder :: (String, Int, Int, String, String) -> Int
dateOrder (date,h,min,_,_) = (44640*(read m))+(1440*(read d))+(60*h)+min
        where
          y:m:d:_ = splitOn "-" date

toSleepTimeSlots :: [(String, Int, Int, String, String)] -> [(String, Int, Int, Int)]
toSleepTimeSlots events = toSleepTimeSlots' events [] "0" 0 0
  where
    toSleepTimeSlots' [] cum _ _ _ = cum
    toSleepTimeSlots' ((date,_,min,event,id):evts) cum gId from to
      | event == "Guard" = toSleepTimeSlots' evts cum id 0 0
      | event == "falls" = toSleepTimeSlots' evts cum gId min 0
      | event == "wakes" = toSleepTimeSlots' evts ((date,read (filter (/='#') gId),from,min-1):cum) gId 0 0
      | True             = cum

perGuard :: [(String, Int, Int, Int)] -> IntMap.IntMap [(Int, Int)]
perGuard slots = IntMap.fromListWith (++) $ map simpler slots
  where
    simpler (date, id, f, t) = (id, [(f, t)])

biggest :: Int -> Int -> (Int, Int) -> (Int, Int)
biggest key a (key', b)
  | a > b = (key, a)
  | True  = (key', b)

mostFrequent :: [(Int, Int)] -> (Int, Int)
mostFrequent ps = IntMap.foldrWithKey biggest (0,0) $ freqMap ps IntMap.empty
  where
    freqMap :: [(Int, Int)] -> IntMap.IntMap Int -> IntMap.IntMap Int
    freqMap [] cum          = cum
    freqMap ((f, t):ps) cum = freqMap ps $ IntMap.unionWith (+) cum $ IntMap.fromAscList [(i,1) | i <- [f..t]]

test = do
    f <- readFile "input4.txt"
    let
      guardData = perGuard $ toSleepTimeSlots $ sortOn dateOrder $ catMaybes $ map getSegments $ lines f
      sleepiestGuard = IntMap.foldrWithKey biggest (0,0) $ IntMap.map (foldr sumTimes 0) guardData
    putStrLn $ show $ sleepiestGuard
    putStrLn $ show $ mostFrequent $ (IntMap.!) guardData (fst sleepiestGuard)
    --part2
    putStrLn $ show $ IntMap.foldrWithKey biggest' (0, (0, 0)) $ IntMap.map mostFrequent guardData
      where
        sumTimes :: (Int, Int) -> Int -> Int
        sumTimes (f, t) sum = sum + (t - f)
        biggest' :: Int -> (Int, Int) -> (Int, (Int, Int)) -> (Int, (Int, Int))
        biggest' k (m, c) (key', (m', c'))
          | c > c' = (k, (m, c))
          | True  = (key', (m', c'))
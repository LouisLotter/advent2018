module Q3 where


import qualified Data.Map.Lazy as Map
import Data.List.Split
import Data.List
import Data.Char
import Data.List.Unique


toVectors :: String -> (String,Char)
toVectors str = case splitOn " " str of
  _:a:_:_:_:_:_:b:_ ->  (a, head b)

root :: [(String, Char)] -> String
root str = sortUniq $ notIn ls rs
  where
    (ls, rs)              = foldr build ("","") str
    build (s, c) (cs, ss) = (c:cs, s++ss)
    notIn str = filter (flip notElem str)

toGraph :: [(String, Char)] -> [(Char, String)]
toGraph ts = Map.toList $ foldr toGraph' Map.empty ts
  where
    toGraph' (v, k) = Map.insertWith addSort k v
    addSort a b     = sortUniq (a ++ b)

doStep :: Char -> [(Char, String)] -> (String, [(Char, String)])
doStep c ts = (map toChar  $ filter (not . depEmpty) depRemoved ,filter depEmpty depRemoved)
  where
    toChar (c, dps)     = c
    depEmpty (c, dps)   = dps /= ""
    depRemoved          = map (removeC c) ts
    removeC c (c', dps) = (c', filter (/= c) dps)

doFirst :: [(Char, String)] -> String -> String -> String
doFirst _ [] done       = reverse done
doFirst ts (a:ava) done = doFirst ts' mav (a:done)
  where
    mav = sortUniq (ava ++ nav)
    (nav, ts') = doStep a ts

test = do
    f <- readFile "input7.txt"
    let
      vs      = map toVectors $ lines f
      initial = root $ vs
      graph   = toGraph $ vs
    putStrLn $ show $ graph
    putStrLn $ show $ initial
    putStrLn $ show $ doFirst graph initial ""



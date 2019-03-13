{-# LANGUAGE BangPatterns #-}

import Data.Function (fix)

doubleList :: Integer -> [Integer] -> [Integer]
doubleList _ [] = []
doubleList m (n:ns) = (m * n) : doubleList m ns

scanSum :: [Integer] -> [Integer]
scanSum = scanSumAux 0
  where
    scanSumAux _ [] = []
    scanSumAux n (x:xs) = sum : scanSumAux sum xs
      where
        sum = x + n

diffList :: [Integer] -> [Integer]
diffList [] = []
diffList [x] = []
diffList (x:y:xs) = y - x : diffList (y:xs)

runLenEng :: [Char] -> [(Integer,Char)]
runLenEng     = runLenEng' 1
  where
    runLenEng' :: Integer -> [Char] -> [(Integer,Char)]
    runLenEng' _ []  = []
    runLenEng' n [x] = [(n,x)]
    runLenEng' n (x:y:xs)
      | (x == y) = runLenEng' (n+1) (y:xs)
      | True     = (n,x) : runLenEng' 1 (y:xs)


--andL :: [Bool] -> Bool
--andL

data Product a b = Product a b
data Sum a b = Sum a | Sum2 b
data Algebraic a b = Sum1 (Sum a b) | Prod (Product a b) (Sum a b)



--f(n) = max(n, f(n/2) + f(n/3) + f(n/4))


f :: (Int -> Int) -> Int -> Int
f mf 0 = 0
f mf n = max n $ mf (n `div` 2) +
                 mf (n `div` 3) +
                 mf (n `div` 4)



--
data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q


nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

f_tree :: Tree Int
f_tree = fmap (f fastest_f) nats

fastest_f :: Int -> Int
fastest_f = index f_tree

k = 1
--again, this won't work as expected
h :: Int -> Bool
h k = True
h _ = False


main = putStrLn "Hello Haskell!"

module Q3 where

import Data.List.Split
import Data.Maybe
import Data.List

type Point = (Int, Int)
data Turn = Left' | Right'
type Instruction = (Turn, Int)
data Direction = West | North | East | South
type GPSReadout = (Point,Direction)


split' :: String -> [String]
split' = splitOn ", "

parseAll :: [String] -> [Instruction]
parseAll = map parse

parse :: String -> Instruction
parse ('L':xs) = (Left',read xs)
parse ('R':xs) = (Right',read xs)

gpsFollow :: GPSReadout -> Instruction -> GPSReadout
gpsFollow ((x, y), North) (Left', d) = ((x-d, y), West)
gpsFollow ((x, y), South) (Left', d) = ((x+d, y), East)
gpsFollow ((x, y), East) (Left', d) = ((x, y+d), North)
gpsFollow ((x, y), West) (Left', d) = ((x, y-d), South)
gpsFollow ((x, y), North) (Right', d) = ((x+d, y), East)
gpsFollow ((x, y), South) (Right', d) = ((x-d, y), West)
gpsFollow ((x, y), East) (Right', d) = ((x, y-d), South)
gpsFollow ((x, y), West) (Right', d) = ((x, y+d), North)

navigateAcc :: [GPSReadout] -> Instruction -> [GPSReadout]
navigateAcc xs@(x:_) i = gpsFollow x i : xs

navigate :: [Instruction] -> [GPSReadout]
navigate = foldl navigateAcc [((0,0), North)]

finalDestination :: [GPSReadout] -> Point
finalDestination g = fst $ head g

distanceToHQ :: Point -> Int
distanceToHQ (x,y) = abs x + abs y

solve :: String -> Int
solve = distanceToHQ . finalDestination . navigate . parseAll . split'

test = do
    f     <- readFile "input2016Q1.txt"
    putStrLn $ show $ solve f

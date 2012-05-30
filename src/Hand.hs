module Hand (Card(..)
  , Rank(..)
  , rank
  , toIndex
  , combinaisonsOfAll
  , probability)

where

import Data.List (sort, group)
import Control.Arrow ((&&&))

data Card = One | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Bishop | Jack | Queen | King | Ace | Daemon
  deriving (Eq, Show, Ord, Bounded, Enum)

data Rank = 
    HighCard Card Card Card Card Card
  | Pair Card Card Card Card
  | TwoPair Card Card Card
  | ThreeOfAKind Card Card Card
  | FullHouse Card Card
  | FourOfAKind Card Card
  | FiveOfAKind Card
  deriving (Eq, Show, Ord)


probability :: Rank -> Int
probability h =
  let allRanks = sort . map (length &&& head) . group . map rank $ combinaisonsOfAll 5
      belowRanks = filter (\x -> snd x < h) allRanks
  in
    foldl (\x y -> x + fst y) 0 belowRanks

toIndex :: Rank -> Int
toIndex (HighCard x y z t u) =
  sumcomb (fromEnum x - 1) 4
  + sumcomb (fromEnum y - 1) 3
  + sumcomb (fromEnum z - 1) 2
  + sumcomb (fromEnum t - 1) 1
  + fromEnum u

toIndex (Pair x y z t) =
  4368
  + fromEnum x * 455
  + sumcomb (fromEnum y - diff x y) 2
  + sumcomb (fromEnum z - diff x z) 1
  + (fromEnum t + 1 - diff x t) 
    where diff :: Card -> Card -> Int
          diff p c
             | c < p = 1
             | otherwise = 2

toIndex (TwoPair x y z) =
  11648
  + (sumcomb (fromEnum x - 1) 1
     + (fromEnum y)) * 14
  + fromEnum z - diff3 x y z
  where diff3 :: Card -> Card -> Card -> Int
        diff3 a b c 
            | c < b = 0
            | c > b && c < a = 1
            | otherwise = 2

toIndex (ThreeOfAKind x y z) =
    13328
    + fromEnum x * 105
    + sumcomb (fromEnum y - diff x y) 1
    + sumcomb (fromEnum z - diff x z) 1
    where diff :: Card -> Card -> Int
          diff a b
            | a > b = 1
            | otherwise = 2

toIndex (FullHouse x y) =
    15008
    + fromEnum x * 16
    + fromEnum y

toIndex (FourOfAKind x y) =
    15248
    + fromEnum x * 16
    + fromEnum y

toIndex (FiveOfAKind x) =
    15488 + fromEnum x



comb :: Int -> Int -> Int
comb n k = product [n-k+1..n] `div` product [1..k] 

sumcomb :: Int -> Int -> Int
sumcomb n k = foldr (+) 0 $ map (flip comb k) [1..n]
  

rank :: [Card] -> Rank
rank h = 
  let g = sort . map (length &&& head) . group . reverse . sort $ h
  in
    case g of
    [(5, x)] -> FiveOfAKind x
    [(1, x), (4, y)] -> FourOfAKind y x
    [(2, x), (3, y)] -> FullHouse y x
    [(1, x), (1, y), (3, z)] -> ThreeOfAKind z y x
    [(1, x), (2, y), (2, z)] -> TwoPair z y x
    [(1, x), (1, y), (1, z), (2, t)] -> Pair t z y x
    [(1, x), (1, y), (1, z), (1, t), (1, u)] -> HighCard u t z y x
    _ -> error $ "not a hand: " ++ show h


combinaisonsOf :: Int -> [a] -> [[a]]
combinaisonsOf 0 _ = [[]]
combinaisonsOf n l = [x:y | x <- l, y <- combinaisonsOf (n - 1) l]

combinaisonsOfAll :: (Bounded a, Enum a) => Int -> [[a]]
combinaisonsOfAll n =
  combinaisonsOf n (enumFromTo minBound maxBound)


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
  comb (fromEnum x - 1) 4
  + comb (fromEnum y - 2) 3
  + comb (fromEnum z - 3) 2
  + comb (fromEnum t - 4) 1

comb :: Int -> Int -> Int
comb n k = product [n-k+1..n] `div` product [1..k] 

sumcomb :: Int -> Int -> Int
sumcomb n k = foldr (+) 0 $ map (flip comb k) [1..n-1]
  

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


module Hand (Card(..), Rank(..), rank)

where

import Data.List (sort, group)

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


rank :: [Card] -> Rank
rank h = 
  let g = sort . map (\x -> (length x, head x)) . group . reverse . sort $ h
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



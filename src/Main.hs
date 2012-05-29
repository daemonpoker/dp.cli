module Main where

import Data.List (sort, group)
import Hand

data MajorRank = MHighCard | MPair | MTwoPair | MThreeOfAKind | MFullHouse | MFourOfAKind | MFiveOfAKind deriving (Show, Eq, Ord)

main :: IO ()
main = do
  putStrLn "Daemon Poker Lab"
  print $ rank [Daemon, Daemon, Daemon, Daemon, Daemon]
  print $ rankCount


rankCount :: [(Int, MajorRank)]
rankCount = map (\x -> (length x, head x)) allRanks
  where
    allHands = combinaisonsOf 5 (enumFromTo One Daemon)
    allRanks = group . sort . map (majorRank . rank) $ allHands

majorRank :: Rank -> MajorRank
majorRank (HighCard _ _ _ _ _) = MHighCard
majorRank (Pair _ _ _ _) = MPair
majorRank (TwoPair _ _ _) = MTwoPair
majorRank (ThreeOfAKind _ _ _) = MThreeOfAKind
majorRank (FullHouse _ _) = MFullHouse
majorRank (FourOfAKind _ _) = MFourOfAKind
majorRank (FiveOfAKind _) = MFiveOfAKind

combinaisonsOf :: Int -> [a] -> [[a]]
combinaisonsOf 0 _ = [[]]
combinaisonsOf _ [] = []
combinaisonsOf n l = [(x:y) | x <- l, y <- combinaisonsOf (n - 1) l]



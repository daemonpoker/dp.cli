module Main where

import Data.List (sort, group, nub)
import Control.Arrow ((&&&))
import qualified Data.Vector as V

import Hand

data MajorRank = MHighCard | MPair | MTwoPair | MThreeOfAKind | MFullHouse | MFourOfAKind | MFiveOfAKind deriving (Show, Eq, Ord)

main :: IO ()
main = do
  putStrLn "Daemon Poker Lab"
  -- print $ rank [Daemon, Daemon, Daemon, Daemon, Daemon]
  -- print rankCount
  print $ V.take 10 checkToIndex 


checkToIndex :: V.Vector Rank
checkToIndex =
    let rs = V.fromList $ map (head) . group . sort . map rank $ combinaisonsOfAll 5
    in V.ifilter (\i r -> i /= toIndex r) rs



rankCount :: [(Int, MajorRank)]
rankCount = map (length &&& head) allRanks
  where
    allRanks = group . sort . map (majorRank . rank) $ combinaisonsOfAll 5

majorRank :: Rank -> MajorRank
majorRank HighCard {} = MHighCard
majorRank Pair {} = MPair
majorRank TwoPair {} = MTwoPair
majorRank ThreeOfAKind {} = MThreeOfAKind
majorRank FullHouse {} = MFullHouse
majorRank FourOfAKind {} = MFourOfAKind
majorRank FiveOfAKind {} = MFiveOfAKind


module Main

where

import Data.List (sort, group)
import qualified Data.Vector as V
import Control.Arrow ((&&&))
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Hand

main :: IO ()
main = defaultMain tests

tests = [testGroup "Hand group"
          [testCase "toIndex" testIndexOf
          ,testCase "rankCount" testRank]]

testIndexOf :: IO ()
testIndexOf =
  let rs = V.fromList $ map (head) . group . sort . map rank $ combinaisonsOfAll 5
      faulty = V.ifilter (\i r -> i /= toIndex r) rs
  in assertBool "Some ranks do not match their index" $ V.null faulty

data MajorRank = MHighCard | MPair | MTwoPair | MThreeOfAKind | MFullHouse | MFourOfAKind | MFiveOfAKind deriving (Show, Eq, Ord)

testRank :: IO ()
testRank = assertEqual "Bad number of ranks" rankCount [(524160, MHighCard)
                                                       ,(436800, MPair)
                                                       ,(50400, MTwoPair)
                                                       ,(33600, MThreeOfAKind)
                                                       ,(2400, MFullHouse)
                                                       ,(1200, MFourOfAKind)
                                                       ,(16, MFiveOfAKind)]

majorRank :: Rank -> MajorRank
majorRank HighCard {} = MHighCard
majorRank Pair {} = MPair
majorRank TwoPair {} = MTwoPair
majorRank ThreeOfAKind {} = MThreeOfAKind
majorRank FullHouse {} = MFullHouse
majorRank FourOfAKind {} = MFourOfAKind
majorRank FiveOfAKind {} = MFiveOfAKind

rankCount :: [(Int, MajorRank)]
rankCount = map (length &&& head) allRanks
  where
    allRanks = group . sort . map (majorRank . rank) $ combinaisonsOfAll 5

combinaisonsOf :: Int -> [a] -> [[a]]
combinaisonsOf 0 _ = [[]]
combinaisonsOf n l = [x:y | x <- l, y <- combinaisonsOf (n - 1) l]

combinaisonsOfAll :: (Bounded a, Enum a) => Int -> [[a]]
combinaisonsOfAll n =
  combinaisonsOf n (enumFromTo minBound maxBound)


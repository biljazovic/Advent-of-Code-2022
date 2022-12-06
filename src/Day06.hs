module Day06 (main06) where

import Util
import qualified Data.Set as Set

solve :: Int -> String -> Int
solve n input = length (takeWhile ((< n) . Set.size . Set.fromList) ns) + n
  where
    ns = transpose $ [ drop i input | i <- [0..(n-1)] ]

main06 :: IO ()
main06 = do
    input <- head . lines <$> readFile "res/input06"
    print $ solve 4 input
    print $ solve 14 input

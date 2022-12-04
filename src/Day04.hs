module Day04 (main04) where

import Util

containing :: [[Int]] -> Bool
containing [p1, p2] = f p1 p2 || f p2 p1
  where
    f [a, b] [c, d] = a <= c && b >= d

overlapping :: [[Int]] -> Bool
overlapping [p1, p2] = f p1 p2 || f p2 p1
  where
    f [a, b] [c, d] = a <= c && c <= b

main04 :: IO ()
main04 = do
    input <- map (map (map read . splitOn "-") . splitOn ",") . lines <$> readFile "res/input04" :: IO [[[Int]]]
    print $ length . filter containing $ input
    print $ length . filter overlapping $ input

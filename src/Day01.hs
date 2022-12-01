module Day01 (main01) where

import Util ( sort, splitOn )

-- $> main01
main01 :: IO ()
main01 = do
    input <- map (map read) . splitOn [""] . lines <$> readFile "res/input01" :: IO [[Int]]
    print $ (maximum . map sum) input
    print $ (sum . take 3 . reverse . sort . map sum) input

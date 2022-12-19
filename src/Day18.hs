module Day18 (main18) where

import Util
import qualified Data.Set as Set

touching d1 d2 = (sum . map abs $ zipWith (-) d1 d2) == 1

solve :: [[Int]] -> Int
solve input = 6*length input - (sum . map f $ input)
  where
    f d = listCount (touching d) input

dirs = [ [1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1] ]

susedi6 :: [Int] -> [[Int]]
susedi6 p = map (zipWith (+) p) dirs

solveB :: [[Int]] -> Int
solveB input = outCount
  where
    lavaSeen = Set.fromList input
    granice = map (\l -> (minimum l, maximum l)) . transpose $ input
    outside p = or $ zipWith (\px (xMin, xMax) -> px < xMin || px > xMax) p granice
    (_, _, outCount) =
      let f p (outSeen', seen', outCount')
            | Set.member p lavaSeen = (outSeen', seen', outCount')
            | Set.member p outSeen' = (outSeen', seen', outCount'+1)
            | Set.member p seen' = (outSeen', seen', outCount')
            | otherwise = let (seen'', isOut) = go (Set.empty, False) p
                              seen''' = seen' `Set.union` seen''
                           in if isOut
                                 then (outSeen' `Set.union` seen'', seen''', outCount'+1)
                                 else (outSeen', seen''', outCount')
       in foldr f (Set.empty, Set.empty, 0) $ concatMap susedi6 input
    go (currSeen, isOut) p
      | Set.member p lavaSeen = (currSeen, isOut)
      | Set.member p currSeen = (currSeen, isOut)
      | outside p = (Set.insert p currSeen, True)
      | otherwise = foldl go (Set.insert p currSeen, isOut) $ susedi6 p

main18 :: IO ()
main18 = do
    input <- map (map read . splitOn ",") . lines <$> readFile "res/input18" :: IO [[Int]]
    print $ solve input
    print $ solveB input

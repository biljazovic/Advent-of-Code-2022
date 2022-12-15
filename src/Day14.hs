module Day14 (main14) where

import Util
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens

solve :: (Int -> Set (V2 Int) -> Set (V2 Int)) -> [[V2 Int]] -> Int
solve addSand input = Set.size (addSand bound ini) - Set.size ini
  where
    ini = foldr insertRock Set.empty input
    insertRock rock s = foldr insertSegment s $ zip rock (tail rock)
    insertSegment (V2 x1 y1, V2 x2 y2) s =
      foldr Set.insert s [V2 a b | a <- [min x1 x2 .. max x1 x2], b <- [min y1 y2 .. max y1 y2]]
    bound = (maximum . map (^._y) . Set.elems $ ini) + 2

addSandA :: Int -> Set (V2 Int) -> Set (V2 Int)
addSandA bound s = case fall (V2 500 0) of
  Just p -> addSandA bound (Set.insert p s)
  Nothing -> s
  where
    fall :: V2 Int -> Maybe (V2 Int)
    fall (V2 a b)
      | b > bound = Nothing
      | Set.notMember (V2 a (b+1)) s = fall (V2 a (b+1))
      | Set.notMember (V2 (a-1) (b+1)) s = fall (V2 (a-1) (b+1))
      | Set.notMember (V2 (a+1) (b+1)) s = fall (V2 (a+1) (b+1))
      | otherwise = Just (V2 a b)

addSandB :: Int -> Set (V2 Int) -> Set (V2 Int)
addSandB bound s = case fall (V2 500 0) of
  Just p -> addSandB bound (Set.insert p s)
  Nothing -> s
  where
    empty p = Set.notMember p s && p^._y /= bound
    fall :: V2 Int -> Maybe (V2 Int)
    fall (V2 a b)
      | Set.member (V2 a b) s = Nothing
      | empty (V2 a (b+1)) = fall (V2 a (b+1))
      | empty (V2 (a-1) (b+1)) = fall (V2 (a-1) (b+1))
      | empty (V2 (a+1) (b+1)) = fall (V2 (a+1) (b+1))
      | otherwise = Just (V2 a b)

main14 :: IO ()
main14 = do
    input <- map (map ((\[a,b] -> V2 a b) . map read . splitOn ",") . splitOn " -> ") . lines <$> readFile "res/input14"
    print $ solve addSandA input
    print $ solve addSandB input

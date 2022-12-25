module Day24 (main24) where

import Util
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Array as Arr

type Matrix = Map (V2 Int) [Char]

toDir = \case
  '>' -> V2 0 1
  '<' -> V2 0 (-1)
  'v' -> V2 1 0
  '^' -> V2 (-1) 0

advance :: (V2 Int, V2 Int) -> Matrix -> Matrix
advance (V2 x1 y1, V2 x2 y2) mat = mat'
  where
    mat' = Map.fromListWith (++) . concatMap f . Map.assocs $ mat
    f (pos, dirs) = map (g . (pos, )) dirs
    g (pos, dir) = 
      let V2 x' y' = pos + toDir dir
          pos'' = if | x' < x1 -> V2 x2 y'
                     | x' > x2 -> V2 x1 y'
                     | y' < y1 -> V2 x' y2
                     | y' > y2 -> V2 x' y1
                     | otherwise -> V2 x' y'
       in (pos'', [dir])

solve :: CharMatrix -> Int
solve input = snd $ solve' startPos endPos matI bounds
  where
    endPos = snd bounds + V2 1 0
    startPos = fst bounds - V2 1 0
    bounds = (\(b1, b2) -> (b1+V2 1 1, b2-V2 1 1)) $ Arr.bounds input
    matI = Map.fromList . filter (\(i,s) -> head s `notElem` "#.") . Arr.assocs . fmap (: []) $ input

solveB :: CharMatrix -> Int
solveB input = l1 + l2 + l3
  where
    (mat1, l1) = solve' startPos endPos matI bounds
    (mat2, l2) = solve' endPos startPos mat1 bounds
    (mat3, l3) = solve' startPos endPos mat2 bounds
    endPos = snd bounds + V2 1 0
    startPos = fst bounds - V2 1 0
    bounds = (\(b1, b2) -> (b1+V2 1 1, b2-V2 1 1)) $ Arr.bounds input
    matI = Map.fromList . filter (\(_,s) -> head s `notElem` "#.") . Arr.assocs . fmap (: []) $ input

solve' :: V2 Int -> V2 Int -> Matrix -> (V2 Int, V2 Int) -> (Matrix, Int)
solve' startPos endPos matI bounds = go (Set.singleton startPos) matI
  where
    go points mat = if Set.member endPos points 
                       then (mat, 0)
                       else let mat' = advance bounds mat
                                points' = Set.fromList . concatMap (step mat') . Set.toList $ points
                             in fmap (+1) $ go points' mat'
    step mat pos = let emptyPos p = p `elem` [startPos,endPos] || (inBounds bounds p) && Map.notMember p mat
                       in filter emptyPos $ pos : (susedi4 Nothing pos)

main24 :: IO ()
main24 = do
    input <- parseMatrix <$> readFile "res/input24"
    print $ solve input
    print $ solveB input

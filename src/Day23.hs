module Day23 (main23) where

import Util
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Arr
import Control.Lens

dirs8 = cycle [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 1, V2 1 1, V2 1 0, V2 1 (-1), V2 0 (-1)]

testOrder = cycle [1, 5, 7, 3]

simulate (mat, order) = (mat', drop 1 order)
  where
    space pos = Set.notMember pos mat
    proposeInDir pos dir_ind =
      if all (space . (+pos)) ([dirs8 !! ind | ind <- [dir_ind-1..dir_ind+1]])
         then Just $ pos + dirs8 !! dir_ind
         else Nothing
    propose pos = if all space (susedi8 Nothing pos)
                     then pos
                     else case mapMaybe (proposeInDir pos) (take 4 order) of
                       (pos' : _) -> pos'
                       [] -> pos
    proposed = Map.fromListWith (++) $ map (\pos -> (propose pos, [pos])) (Set.toList mat)
    mat' = Set.fromList $ concatMap (\(pos', poss) -> if length poss == 1 then [pos'] else poss) (Map.assocs proposed)

score mat = (maxX - minX + 1) * (maxY - minY + 1) - Set.size mat
  where
    maxX = maximum . map (^._x) . Set.toList $ mat
    minX = minimum . map (^._x) . Set.toList $ mat
    maxY = maximum . map (^._y) . Set.toList $ mat
    minY = minimum . map (^._y) . Set.toList $ mat

solve matc = score matF
  where
    matF = fst $ iterate simulate (mat, testOrder) !! 10
    mat = Set.fromList . map fst . filter ((=='#') . snd) . Arr.assocs $ matc

solveB matc = length (iterateUntil (\((m1,_),(m2,_)) -> m1==m2) simulate (mat, testOrder)) - 1
  where
    mat = Set.fromList . map fst . filter ((=='#') . snd) . Arr.assocs $ matc

main23 :: IO ()
main23 = do
    input <- parseMatrix <$> readFile "res/input23"
    print $ solve input
    print $ solveB input

module Day12 (main12) where

import Util
import qualified Data.Array as Arr

fix = \case
  'S' -> 'a'
  'E' -> 'z'
  c   -> c

solve :: (Char -> Bool) -> CharMatrix -> Maybe Int
solve startTest mat = dijkstra start goal neigh
  where
    goal = (== 'E') . (mat Arr.!)
    start = map fst . filter (startTest . snd) $ Arr.assocs mat
    neigh p = map (,1) $ filter (good p) (susedi4 (Just $ Arr.bounds mat) p)
    good p p' = fix (mat Arr.! p') <= succ (fix $ mat Arr.! p)

main12 :: IO ()
main12 = do
    input <- parseMatrix <$> readFile "res/input12"
    print $ solve (== 'S') input
    print $ solve ((== 'a') . fix) input

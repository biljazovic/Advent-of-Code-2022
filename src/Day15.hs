module Day15 (main15) where

import Util
import Data.Maybe (mapMaybe)
import Text.Scanf
import Control.Lens

parsePoss = mapMaybe $ \str -> do
  (x1 :+ (y1 :+ (x2 :+ (y2 :+ ())))) <- scanf [fmt|Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d|] str
  return (V2 x1 y1, V2 x2 y2)

sY = 2000000

segment y (V2 xs ys, V2 xb yb) =
  let razy = abs (ys - y)
      dist = abs (xs - xb) + abs (ys - yb)
      razx = dist - razy
   in if razy > dist
         then Nothing
         else Just (xs - razx, xs + razx)

type Seg = (Int, Int)

addSeg :: Seg -> [Seg] -> [Seg]
addSeg seg [] = [seg]
addSeg seg@(x1, x2) (seg'@(x1', x2') : segs)
  | x2 < x1' = seg : (seg' : segs)
  | x1 > x2' = seg' : addSeg seg segs
  | otherwise = addSeg (min x1 x1', max x2 x2') segs

makeSegsAtY y input = foldr addSeg [] $ mapMaybe (segment y) input

solve input = (sum . map segLen $ mergedSegs) - sub
  where
    mergedSegs = makeSegsAtY sY input
    segLen (a, b) = b - a + 1
    sub = length . nub . filter ((== sY) . (^._y)) . concatMap (\(x, y) -> [x, y]) $ input

solveB input = fmap f seg
  where
    f ([(_, (+1)->x), _], y) = x*2*sY + y
    seg = find ((>1) . length . fst) . map (\y -> (makeSegsAtY y input, y)) $ [0..2*sY]

main15 :: IO ()
main15 = do
    input <- parsePoss . lines <$> readFile "res/input15"
    print $ solve input
    print $ solveB input

module Day08 (main08) where

import Util
import qualified Data.Array as Arr

solve :: CharMatrix -> Int
solve mat = listCount visible (Arr.indices mat)
  where
    visible p = any (visInDir p) [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]
    visInDir p dir = all ((< (mat Arr.! p)) . (mat Arr.!))
      $ takeWhile (inBounds (Arr.bounds mat))
      $ iterate (+ dir) (p+dir)

solveB :: CharMatrix -> Int
solveB mat = maximum . map score $ Arr.indices mat
  where
    score p = product $ map (scoreInDir p) [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]
    scoreInDir p dir =
      let f p' = inBounds (Arr.bounds mat) p' && (mat Arr.! p > mat Arr.! p')
          visible = takeWhile f (iterate (+dir) (p+dir))
       in length visible + if (not . null) visible && Arr.bounds mat `inBounds` (last visible + dir) then 1 else 0

main08 :: IO ()
main08 = do
    input <- parseMatrix <$> readFile "res/input08"
    print $ solve input
    print $ solveB input

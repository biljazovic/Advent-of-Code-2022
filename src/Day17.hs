module Day17 (main17) where

import Util
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.RawString.QQ
import Control.Lens

type Rock = [V2 Int]

rocksRaw = [r|
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
|]

wid = 7

fromRockRaw :: String -> Rock
fromRockRaw = concatMap (\(y, s) -> mapMaybe
  (\(x, c) -> if c == '#' then Just (V2 x y) else Nothing)
  (zip [0..] s))
    . zip [0..] . reverse . lines

rocks :: [Rock]
rocks = map (fromRockRaw .  trim) $ splitOn "\n\n" rocksRaw

fallRock :: Rock -> String -> Int -> Set (V2 Int) -> (Set (V2 Int), Int)
fallRock rock wind w mapa = (rockSet endPos `Set.union` mapa, w')
  where
    (endPos, w') = go startPos w
    startPos = V2 3 (4 + height mapa)
    rockSet pos = Set.fromList $ map (+pos) rock
    move :: V2 Int -> V2 Int -> V2 Int
    move dir pos = let moved = rockSet (pos + dir)
                    in if Set.size (moved `Set.intersection` mapa) > 0 ||
                          any (\(V2 x _) -> x <= 0 || x > wid) (Set.toList moved)
                          then pos
                          else pos + dir
    go :: V2 Int -> Int -> (V2 Int, Int)
    go pos w =
      let pos' = move (dirFromWind w) pos
          pos'' = move (V2 0 (-1)) pos'
          w' = (w + 1) `mod` (length wind)
       in if pos' == pos''
             then (pos', w')
             else go pos'' w'
    dirFromWind w = case (wind !! w) of
                      '>' -> V2 1 0
                      '<' -> V2 (-1) 0

streamOfStates :: String -> [(Set (V2 Int), Int)]
streamOfStates input = scanl fall (initialMap, 0) $ concat (repeat rocks)
  where
    initialMap = Set.fromList [ V2 i 0 | i <- [1..7] ]
    fall (mapa, w) rock = fallRock rock input w mapa

height :: Set (V2 Int) -> Int
height = maximum . map (^._y) . Set.toList

solve :: String -> Int
solve input = height finalMap
  where
    finalMap = fst $ streamOfStates input !! len1

len1 = 2022
len2 = 1000000000000

solveB :: String -> Int
solveB input = f r1 + (f r2 - f r1) * numPeriods + f (r1 + numOstatak) - f r1
  where
    f r = height (fst (streamOfStates input !! r))
    (numPeriods, numOstatak) = (len2 - r1) `divMod` period
    period = r2 - r1
    (r2, r1) = go Map.empty (zip [0..] $ streamOfStates input)
    go seenProfils ((rInd, (mapa, w)) : states) =
      let key = (profil mapa, w, rInd `mod` length rocks)
       in case seenProfils Map.!? key of
             Just rInd' -> (rInd, rInd')
             Nothing -> go (Map.insert key rInd seenProfils) states

profil :: Set (V2 Int) -> [Int]
profil mapa = map ((yM -) . (\x -> height . Set.filter ((==x) . (^._x)) $ mapa)) [1..7]
  where
    yM = height mapa

main17 :: IO ()
main17 = do
    input <- head . lines <$> readFile "res/input17"
    print $ solve input
    print $ solveB input

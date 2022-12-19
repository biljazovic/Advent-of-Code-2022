module Day16 (main16) where

import Util
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.ParserCombinators.ReadP
import Data.Function.Memoize
import Data.Bits
import Data.Word

parser = do
  so <- string "Valve " *> manyTill get space  
  r <- string "has flow rate=" *> parseInt
  gos <- (string "; tunnels lead to valves " +++ string "; tunnel leads to valve ") *> many1 get
  return (so, (r, splitOn ", " gos))

type IT = [(String, (Int, [String]))]

mkGraf :: IT -> ([Int], IntMap [(Int, Int)])
mkGraf input = (map (fst . (mapa Map.!)) vs, IntMap.fromList $ zip [0..] $ map f vs)
  where
    f v = map (\v' -> (g v v', toInd Map.! v')) $ filter (/= v) vs
    g v v' = fromJust $ genericBfs (== v') (snd . (mapa Map.!)) [v]
    mapa = Map.fromList input 
    toInd = Map.fromList $ zip vs [0..]
    vs = nub . ("AA" :) . map fst $ filter ((> 0) . fst . snd) input

solve input = go' 30 0 0
  where
    (rs, graf) = mkGraf input
    go' :: Int -> Int -> Word16 -> Int
    go' = memoFix3 $ \go timeLeft currPos opened ->
      let r = rs !! currPos 
          gos = graf IntMap.! currPos
          rez1 = if r == 0 || testBit opened (currPos-1)
                    then [] 
                    else [go (timeLeft-1) currPos (setBit opened (currPos-1)) + (timeLeft-1) * r]
          rez2 = flip map gos $ \(d, g) -> if timeLeft < d then 0 else go (timeLeft-d) g opened
       in if timeLeft == 0
             then 0
             else maximum (rez1 ++ rez2)

solveB input = maximum $ flip map [0..fullMask] $ \mask -> go' 26 0 mask + go' 26 0 (fullMask - mask) 
  where
    fullMask = (1 `shiftL` (length rs - 1)) - 1 :: Word16
    (rs, graf) = mkGraf input
    go' :: Int -> Int -> Word16 -> Int
    go' = memoFix3 $ \go timeLeft currPos opened ->
      let r = rs !! currPos 
          gos = graf IntMap.! currPos
          rez1 = if r == 0 || testBit opened (currPos-1)
                    then [] 
                    else [go (timeLeft-1) currPos (setBit opened (currPos-1)) + (timeLeft-1) * r]
          rez2 = flip map gos $ \(d, g) -> if timeLeft < d then 0 else go (timeLeft-d) g opened
       in if timeLeft == 0
             then 0
             else maximum (rez1 ++ rez2)

main16 :: IO ()
main16 = do
    input <- map (fst . head . readP_to_S (parser <* eof)) . lines <$> readFile "res/input16"
    print $ solve input
    print $ solveB input

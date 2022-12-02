module Day02 (main02) where

import Util
import Data.Map (Map)
import qualified Data.Map as Map

data RPS = R | P | S deriving (Ord, Eq)

fromCharL :: Map Char RPS
fromCharL = Map.fromList $ [ ('A',R),('X',R),('B',P),('Y',P),('C',S),('Z',S) ]

fromChar :: Char -> RPS
fromChar = (Map.!) fromCharL

winning = [ (R, S), (P, R), (S, P) ]

score1 :: RPS -> RPS -> Int
score1 a b
  | a == b = 3
  | (a, b) `elem` winning = 0
  | otherwise = 6

score2 :: RPS -> Int
score2 R = 1
score2 P = 2
score2 S = 3

solveB :: RPS -> Char -> RPS
solveB a 'Y' = a
solveB a 'X' = Map.fromList winning Map.! a
solveB a 'Z' = Map.fromList (map swap winning) Map.! a

solveA :: [(RPS, RPS)] -> Int
solveA = sum . map (\(a, b) -> score2 b + score1 a b)

main02 :: IO ()
main02 = do
    input <- map (map head . splitOn " ") . filter (not . emptyLine) . lines <$> readFile "res/input02"
    print $ solveA $ map ((\[a,b] -> (a, b)) . map fromChar) input
    print $ solveA $ map (\[a,b] -> (fromChar a, solveB (fromChar a) b)) $ input


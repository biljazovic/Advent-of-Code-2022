module Day22 (main22) where

import Util
import qualified Data.Map as Map
import qualified Data.Array as Arr
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

data Instr = Go Int | TurnR | TurnL deriving Show

parseInstr :: String -> [Instr]
parseInstr = fst . head . readP_to_S (p <* skipSpaces <* eof)
  where
    p = many1 $ (Go <$> parseInt) <|> (TurnR <$ char 'R') <|> (TurnL <$ char 'L')

type State = (V2 Int, V2 Int)

rotC :: V2 Int -> V2 Int
rotC = \case
  (V2 0 1) -> (V2 1 0)
  (V2 1 0) -> (V2 0 (-1))
  (V2 0 (-1)) -> (V2 (-1) 0)
  (V2 (-1) 0) -> V2 0 1

dirToInt = \case
  (V2 0 1) -> 0
  (V2 1 0) -> 1
  (V2 0 (-1)) -> 2
  (V2 (-1) 0) -> 3

solve followInstr mat instrs = 1000*(x+1) + 4*(y+1) + dirToInt dir
  where
    (V2 x y, dir) = foldl (flip (followInstr mat)) (findFirstFull (V2 0 0) (V2 0 1), V2 0 1) instrs
    findFirstFull pos dir = head . dropWhile ((== ' ') . (mat Arr.!)) $ iterate (+dir) pos

ugh = Map.fromList . concat $ [concatMap (\x -> [x, swap x]) [
  ((V2 0 (50+x), V2 1 0), (V2 (150+x) 0, V2 0 1)), 
  ((V2 0 (100+x), V2 1 0), (V2 199 (0+x), V2 (-1) 0)),
  ((V2 (0+x) 149, V2 0 (-1)), (V2 (149-x) 99, V2 0 (-1))),
  ((V2 49 (100+x), V2 (-1) 0), (V2 (50+x) 99, V2 0 (-1))),
  ((V2 149 (50+x), V2 (-1) 0), (V2 (150+x) 49, V2 0 (-1))),
  ((V2 (0+x) 50, V2 0 1), (V2 (149-x) 0, V2 0 1)),
  ((V2 (50+x) 50, V2 0 1), (V2 100 (0+x), V2 1 0))]
    | x <- [0..49] ]

followInstr wrapF mat instr (pos, dir) = case instr of
  Go x -> (iterate move (pos, dir)) !! x
  TurnL -> (pos, rotC (rotC (rotC dir)))
  TurnR -> (pos, rotC dir)
  where
    full pos = (inBounds (Arr.bounds mat) pos) && (mat Arr.! pos /= ' ')
    move (pos, dir) = 
      let pos' = pos+dir
       in if | not (full pos') -> let (pos'', dir') = wrapF full (pos, dir)
                                   in if mat Arr.! pos'' == '.'
                                         then (pos'', dir')
                                         else (pos, dir)
             | mat Arr.! pos' == '#' -> (pos, dir)
             | otherwise -> (pos', dir)


wrapFa full (pos, dir) = (findLastFull pos (rotC (rotC dir)), dir)
  where
    findLastFull pos dir = last $ takeWhile full $ iterate (+dir) pos

wrapFb _ (pos, dir) = ugh Map.! (pos, (rotC (rotC dir)))

main22 :: IO ()
main22 = do
    [(parseMatrix -> mat), (parseInstr -> instrs)] <- splitOn "\n\n" <$> readFile "res/input22"
    print $ solve (followInstr wrapFa) mat instrs
    print $ solve (followInstr wrapFb) mat instrs

module Day09 (main09) where

import Util
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Command = (Char, Int)
type State = ([V2 Int], Set (V2 Int))

dirFromChar :: Char -> V2 Int
dirFromChar = \case
  'U' -> V2 0 1
  'L' -> V2 (-1) 0
  'R' -> V2 1 0
  'D' -> V2 0 (-1)

touching :: V2 Int -> V2 Int -> Bool
touching (V2 x1 y1) (V2 x2 y2) = max (abs (x1-x2)) (abs (y1-y2)) <= 1

move :: V2 Int -> V2 Int -> V2 Int
move r0' r1 = if touching r0' r1 
                 then r1
                 else fromJust $ find (touching r1) 
                               $ [r0'] ++ (susedi4 Nothing r0') ++ (susedi8 Nothing r0')

exeCmd :: Int -> Command -> State -> State
exeCmd ropeLen (dirFromChar -> dir, len) = (!! len) . iterate exeCmdUnitLen
  where
    exeCmdUnitLen (rope, vis) = 
      let head' = head rope + dir
          rope' = go head' rope
          tail' = last rope'
       in (rope', Set.insert tail' vis)
      where
        go :: V2 Int -> [V2 Int] -> [V2 Int]
        go r0' [r0] = [r0']
        go r0' (r0:r1:rest) = r0' : go (move r0' r1) (r1:rest)

solve :: Int -> [Command] -> Int
solve ropeLen commands = Set.size . snd $ 
  foldl (flip (exeCmd ropeLen)) (replicate ropeLen (V2 0 0), Set.singleton (V2 0 0)) commands

main09 :: IO ()
main09 = do
    input <- map ((\[ch, l] -> (head ch, read l)) . splitOn " ") . lines <$> readFile "res/input09" :: IO [Command]
    print $ solve 2 input
    print $ solve 10 input

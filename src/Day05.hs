module Day05 (main05) where

import Util
import Data.Maybe (mapMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Scanf

data Command = C { amount :: Int, from :: Int, to :: Int } deriving Show

parseCommands = mapMaybe $ \str -> do
  (n :+ (x :+ (y :+ ()))) <- scanf [fmt|move %d from %d to %d|] str
  return $ C n x y

execute :: IntMap String -> Command -> IntMap String
execute sts (C n x y) = IntMap.adjust (reverse toadd ++) y $ IntMap.adjust (drop n) x sts
  where
    toadd = take n $ sts IntMap.! x

execute2 :: IntMap String -> Command -> IntMap String
execute2 sts (C n x y) = IntMap.adjust (toadd ++) y $ IntMap.adjust (drop n) x sts
  where
    toadd = take n $ sts IntMap.! x

main05 :: IO ()
main05 = do
    input <- lines <$> readFile "res/input05"
    let initialState =
          IntMap.fromAscList $ 
            zip [1..] . map ((map (!! 1) . (reverse . drop 1 . reverse)) . filter (not . all (== ' '))) .
            transpose . map (groupByn 4) . takeWhile (not . emptyLine) $ input
    let commands = parseCommands . dropWhile (not . emptyLine) $ input
    print $ map head . IntMap.elems $ foldl execute initialState commands
    print $ map head . IntMap.elems $ foldl execute2 initialState commands

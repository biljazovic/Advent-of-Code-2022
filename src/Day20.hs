module Day20 (main20) where

import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

score seq = sum $ map getAt [1000,2000,3000]
  where
    getAt d = snd . fromJust $ Seq.lookup ((zeroPos+d) `mod` Seq.length seq) seq
    zeroPos = fromJust $ Seq.findIndexL ((== 0) . snd) seq

mix seq = foldl (flip move) seq [0..l-1]
  where
    l = Seq.length seq
    move i seq = let pos = fromJust $ Seq.findIndexL ((== i) . fst) seq
                     d = snd $ fromJust $ Seq.lookup pos seq
                     pos' = ((pos + d) `mod` (l-1) + (l-1)) `mod` (l-1)
                  in Seq.insertAt pos' (i, d) (Seq.deleteAt pos seq)

solve input = score (mix seqI)
  where
    seqI = Seq.fromList $ zip [0..] input

key = 811589153
solveB input = score (iterate mix seqI !! 10)
  where
    seqI = Seq.fromList $ zip [0..] $ map (*key) input

main20 :: IO ()
main20 = do
    input <- map read . lines <$> readFile "res/input20" :: IO [Int]
    print $ solve input
    print $ solveB input

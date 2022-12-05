module Day03 (main03) where

import Util
import qualified Data.Set as Set
import Data.Char (ord, isAsciiLower, isAsciiUpper)

priority :: Char -> Int
priority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 1 + 26

-- $> main03
main03 :: IO ()
main03 = do
    input <- lines <$> readFile "res/input03"
    let score = sum . map (priority . Set.findMin . foldl1 Set.intersection . map Set.fromList)
    print $ score . map (\l -> groupByn (length l `div` 2) l) $ input
    print $ score . groupByn 3 $ input

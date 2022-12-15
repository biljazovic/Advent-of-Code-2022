module Day13 (main13) where

import Util
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

data Packet = Number Int | List [Packet] deriving (Show, Eq)

instance Ord Packet where
  Number a <= Number b = a <= b
  List l1 <= List l2 = l1 <= l2
  Number a <= List l = [Number a] <= l
  List l <= Number a = l <= [Number a]

parsePacket :: ReadP Packet
parsePacket = (Number <$> parseInt) <|>
  List <$> between (char '[') (char ']') (sepBy parsePacket (char ','))

main13 :: IO ()
main13 = do
    let f = fst . head . readP_to_S (parsePacket <* eof)
    input <- map (map f . lines) . splitOn "\n\n" <$> readFile "res/input13"
    print $ sum . map snd . filter ((\[a,b] -> a <= b) . fst) $ zip input [1..] 
    let dividers = [List [List [Number 2]], List [List [Number 6]]]
        sorted = sort . (++dividers) . concat $ input
    print $ product . map (+1) . findIndices (`elem` dividers) $ sorted

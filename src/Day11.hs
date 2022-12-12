module Day11 (main11) where

import Util
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

data Token = Old | Number Int deriving Show
data Operation = Operation { _tokenl :: Token, _operator :: Char, _tokenr :: Token }  deriving Show
data Monkey = Monkey {
  _items :: [Int],
  _operation :: Operation,
  _test :: Int,
  _ifTrue :: Int,
  _ifFalse :: Int,
  _noInspect :: Int } deriving Show

parseOperation :: ReadP Operation
parseOperation = Operation <$> parseToken <*> parseOperator <*> parseToken
  where
    parseToken = (Old <$ string "old") <|> (Number <$> parseInt)
    parseOperator = char ' ' *> get <* char ' '

parseMonkey :: ReadP Monkey
parseMonkey = do
  string "Monkey " *> parseInt <* string ":"
  items <- skipSpaces *> string "Starting items: " *> sepBy parseInt (string ", ")
  operation <- skipSpaces *> string "Operation: new = " *> parseOperation
  test <- skipSpaces *> string "Test: divisible by " *> parseInt
  ifTrue <- skipSpaces *> string "If true: throw to monkey " *> parseInt
  ifFalse <- skipSpaces *> string "If false: throw to monkey " *> parseInt <* skipSpaces <* skipSpaces
  return $ Monkey items operation test ifTrue ifFalse 0

evalOperation :: Operation -> Int -> Int
evalOperation (Operation tl op tr) old = op_f (value tl) (value tr)
  where
    op_f = case op of
             '+' -> (+)
             '*' -> (*)
    value Old = old
    value (Number x) = x

type Monkeys = (IntMap Monkey, Int)

doMonkey :: Bool -> Int -> Monkeys -> Monkeys
doMonkey shouldDiv i (ms, testProd) = (IntMap.insert i m' $ foldl throw ms (_items m), testProd)
  where
    m = ms IntMap.! i
    m' = m { _items = [], _noInspect = _noInspect m + length (_items m) }
    throw ms' item = let item' = evalOperation (_operation m) item `mod` testProd
                         item'' = if shouldDiv then item' `div` 3 else item'
                         whom = if item'' `mod` _test m == 0 then _ifTrue m else _ifFalse m
                      in IntMap.adjust (addToMonkey item'') whom ms'
    addToMonkey item m' = m' { _items = item : _items m' }

doRound :: Bool -> Monkeys -> Monkeys
doRound shouldDiv ms = foldl (flip (doMonkey shouldDiv)) ms (IntMap.keys (fst ms))

solve :: Monkeys -> Int
solve ms = product . take 2 . reverse . sort . map _noInspect . IntMap.elems . fst $
  iterate' (doRound True) ms !! 20

solveB :: Monkeys -> Int
solveB ms = product . take 2 . reverse . sort . map _noInspect . IntMap.elems . fst $
  iterate' (doRound False) ms !! 10000

-- $> main11
main11 :: IO ()
main11 = do
    input <- fst . head . readP_to_S (many parseMonkey <* eof) <$> readFile "res/input11"
    let monkeys = (IntMap.fromList . zip [0..] $ input, product . map _test $ input)
    print $ solve monkeys
    print $ solveB monkeys

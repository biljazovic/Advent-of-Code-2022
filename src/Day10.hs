module Day10 (main10) where

import Util
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

data Command = Addx Int | Noop deriving Show
data State = State { _valx :: Int, _hist :: [Int] }

initialState = State 1 [1]

parseCommand :: ReadP Command
parseCommand = (Noop <$ string "noop") <|> (Addx <$> (string "addx " *> parseInt))

exeCmd :: Command -> State -> State
exeCmd Noop (State val hist) = State val (val : hist)
exeCmd (Addx x) (State val hist) = State (val + x) ((val+x) : val : hist)

solve :: [Command] -> Int
solve cmds = sum . map f $ [20, 60, 100, 140, 180, 220]
  where
    f p = p * (hist !! p)
    hist = reverse . _hist $ foldl (flip exeCmd) initialState cmds

solveB :: [Command] -> String
solveB cmds = intercalate "\n" (groupByn 40 rawOut)
  where
    rawOut = map f [0..6*40-1]
    f i = let i' = i `mod` 40
              s = hist !! i
           in if abs (i'-s) <= 1 then '#' else '.'
    hist = reverse . _hist $ foldl (flip exeCmd) initialState cmds

main10 :: IO ()
main10 = do
    input <- fst . head . readP_to_S (many (parseCommand <* newline) <* eof) <$> readFile "res/input10"
    print $ solve input
    putStrLn $ solveB input

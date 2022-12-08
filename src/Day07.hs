module Day07 (main07) where

import Util
import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes)
import Control.Applicative hiding (many)
import Control.Monad (when)

data Thing = File { _size::Int, _name:: String} | Dir { _name::String, _ls::[Thing] }
  deriving Show

parseName :: ReadP String
parseName = munch1 (`elem` ['a'..'z']++"/.")

parseFile :: ReadP Thing
parseFile = do
  size <- parseInt <* char ' '
  name <- parseName <* newline
  return $ File size name

parseDir :: ReadP ()
parseDir = string "dir" *> munch1 (/= '\n') *> newline $> ()

-- assumes we go through file tree in preorder
parseThing :: ReadP Thing
parseThing = do
  name <- string "$ cd " *> parseName <* newline
  when (name == "..") pfail
  string "$ ls\n"
  ls <- catMaybes <$> many1 ((Just <$> parseFile) <|> (parseDir $> Nothing))
  ls2 <- many parseThing
  string "$ cd ..\n" $> () <|> eof
  return $ Dir name (ls2++ls)


solve :: Thing -> Int
solve = snd . go
  where
    go (File sz _) = (sz, 0)
    go (Dir _ ls) = let gos = map go ls
                        tot = sum . map fst $ gos
                     in (tot, (sum . map snd $ gos) + (if tot <= 100000 then tot else 0))

solveB :: Thing -> Int
solveB thing = let tot = fst . go 0 $ thing
                in snd $ go (neSpace-(totSpace-tot)) thing
  where
    go _ (File sz _) = (sz, totSpace)
    go l (Dir _ ls) = let gos = map (go l) ls
                          tot = sum . map fst $ gos
                          mini = minimum . map snd $ gos
                       in (tot, if tot >= l then min tot mini else mini)
    totSpace = 70000000
    neSpace = 30000000

main07 :: IO ()
main07 = do
    input <- readFile "res/input07"
    let tree = fst . head $ readP_to_S parseThing input
    print $ solve tree
    print $ solveB tree

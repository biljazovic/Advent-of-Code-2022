module Day25 (main25) where

fromCh = \case
  '2' -> 2
  '1' -> 1
  '0' -> 0
  '-' -> -1
  '=' -> -2

fromSnafu :: String -> Integer
fromSnafu str = foldl (\s c -> 5*s + fromCh c) 0 str

toSnafu :: Integer -> String
toSnafu num = reverse $ go num
  where
    go 0 = []
    go x = let (c, r) = case x `mod` 5 of
                          0 -> ('0', 0)
                          1 -> ('1', 0)
                          2 -> ('2', 0)
                          3 -> ('=', 1)
                          4 -> ('-', 1)
            in c : go ((x `div` 5) + r)

solve :: [String] -> String
solve = toSnafu . sum . map fromSnafu

main25 :: IO ()
main25 = do
    input <- lines <$> readFile "res/input25"
    print $ solve input

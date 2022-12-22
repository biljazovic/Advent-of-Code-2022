module Day21 (main21) where

import Util
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Function.Memoize (memoFix)

data Operation = Operation { _monkeyL :: String, _operator :: Char, _monkeyR :: String }  deriving Show

parseName :: ReadP String
parseName = munch1 isAlphaNum

parseOperation :: ReadP Operation
parseOperation = Operation <$> parseName <*> parseOperator <*> parseName
  where
    parseOperator = char ' ' *> get <* char ' '

parseMonkey :: ReadP (String, Either Operation Int)
parseMonkey = do
  name <- parseName <* string ": "
  shout <- (Left <$> parseOperation) <|> (Right <$> parseInt)
  return (name, shout)

op_f = \case
  '*' -> (*)
  '+' -> (+)
  '-' -> (-)
  '/' -> div

solve mapa = go' "root"
  where
    go' = memoFix $ \go name ->
      case mapa Map.! name of
        Left (Operation m1 (op_f -> op) m2) -> op (go m1) (go m2)
        Right d -> fromIntegral d

data Val = Number Integer | Var | OperationB Val Char Val

varval :: Integer -> Val  -> Integer
varval rez (OperationB (Number d) op v) = 
  let d' = case op of
            '+' -> rez - d
            '-' -> d - rez
            '*' -> rez `div` d
            '/' -> d `div` rez
   in varval d' v
varval rez (OperationB v op (Number d)) = 
  let d' = case op of
            '+' -> rez - d
            '-' -> d + rez
            '*' -> rez `div` d
            '/' -> d * rez
   in varval d' v
varval rez Var = rez

solveB mapa = varval eq_num eq_var
  where
    (eq_var, eq_num) = case (go eq1, go eq2) of
                             (Number d, v) -> (v, d)
                             (v, Number d) -> (v, d)
    Left (Operation eq1 _ eq2) = mapa Map.! "root"
    go "humn" = Var
    go name =
      case mapa Map.! name of
        Left (Operation m1 op m2) -> case (go m1, go m2) of
                                       (Number d1, Number d2) -> Number $ (op_f op) d1 d2
                                       (v1, v2) -> OperationB v1 op v2
        Right d -> Number (fromIntegral d)

main21 :: IO ()
main21 = do
    input <- Map.fromList . map (fst . head . readP_to_S (parseMonkey <* eof)) . lines <$> readFile "res/input21"
    print $ solve input
    print $ solveB input

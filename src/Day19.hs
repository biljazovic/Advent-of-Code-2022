module Day19 (main19) where

import Util
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Text.Scanf
import Data.LinearProgram.Common (Constraint(..), LP (..), Direction (..), linCombination, Bounds (..), VarKind (..))
import Data.LinearProgram (writeLP)
import System.Process (readProcess)

parseBluePrints = mapMaybe $ \str -> do
  (id :+ (ore_ore :+ (clay_ore :+ (obs_ore :+ (obs_clay :+ (geo_ore :+ (geo_obs :+ ()))))))) <- scanf [fmt|Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.|] str
  return (ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs)

ores = ["ore", "clay", "obs", "geo"]
ores_p = map (++ "_p") ores
ores_pp = map (++ "p") ores_p

lp :: Int -> (Int, Int, Int, Int, Int, Int) -> LP (String, Int) Int
lp time (ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs) = LP {
  direction = Max,
  objective = linCombination [(1, ("geo",time))],
  constraints = 
    [Constr Nothing 
      (linCombination $ [(1, (ore_pp, t)) | ore_pp <- ores_pp])
      (UBound 1) 
        | t <- [1..time-1]] ++
    [Constr Nothing
      (linCombination [(1, ("ore",t+1)),(-1, ("ore",t)),(-1, ("ore_p",t)),(ore_ore, ("ore_pp",t)),(clay_ore, ("clay_pp",t)),(obs_ore, ("obs_pp",t)),(geo_ore, ("geo_pp",t))])
      (Equ 0)
        | t <- [1..time-1]] ++
    [Constr Nothing
      (linCombination [(1, ("clay",t+1)),(-1, ("clay",t)),(-1, ("clay_p",t)),(obs_clay, ("obs_pp",t))])
      (Equ 0)
        | t <- [1..time-1]] ++
    [Constr Nothing
      (linCombination [(1, ("obs",t+1)),(-1, ("obs",t)),(-1, ("obs_p",t)),(geo_obs, ("geo_pp",t))])
      (Equ 0)
        | t <- [1..time-1]] ++
    [Constr Nothing
      (linCombination [(1, ("geo",t+1)),(-1, ("geo",t)),(-1, ("geo_p",t))])
      (Equ 0)
        | t <- [1..time-1]] ++
    [Constr Nothing
      (linCombination [(1, (ore_p,t+1)), (-1, (ore_p,t)), (-1, (ore_pp,t))])
      (Equ 0) 
        | (ore_p, ore_pp) <- zip ores_p ores_pp, t <- [1..time-1]] ++ 
    concat [[Constr Nothing
      (linCombination [(1, ("ore",t)),(-ore_ore, ("ore_pp",t))])
      (LBound 0),
     Constr Nothing
      (linCombination [(1, ("ore",t)),(-clay_ore, ("clay_pp",t))])
      (LBound 0),
     Constr Nothing
      (linCombination [(1, ("ore",t)),(-obs_ore, ("obs_pp",t))])
      (LBound 0),
     Constr Nothing
      (linCombination [(1, ("clay",t)),(-obs_clay, ("obs_pp",t))])
      (LBound 0),
     Constr Nothing
      (linCombination [(1, ("ore",t)),(-geo_ore, ("geo_pp",t))])
      (LBound 0),
     Constr Nothing
      (linCombination [(1, ("obs",t)),(-geo_obs, ("geo_pp",t))])
      (LBound 0)]
        | t <- [1..time-1] ],
  varBounds = Map.fromList $ 
    (map (, LBound 0) $ concat [[(ore, t), (ore_p, t), (ore_pp, t)] | (ore, ore_p, ore_pp) <- zip3 ores ores_p ores_pp, t <- [2..time]]) ++
    [(("ore",1),Equ 1),(("geo",1),Equ 0),(("obs",1),Equ 0),(("clay",1),Equ 0),
     (("ore_p",1),Equ 1),(("geo_p",1),Equ 0),(("obs_p",1),Equ 0),(("clay_p",1),Equ 0),
     (("ore_pp",1),Equ 0),(("geo_pp",1),Equ 0),(("obs_pp",1),Equ 0),(("clay_pp",1),Equ 0)],
  varTypes = Map.fromList $ (map (, IntVar) $ concat [[(ore, t), (ore_p, t)] | (ore, ore_p) <- zip ores ores_p, t <- [1..time]]) ++ (map (, BinVar) $ [(ore_pp, t) | ore_pp <- ores_pp, t <- [1..time]])
}

solveOne time i = do
  writeLP "out.lp" (lp time i)
  out <- readProcess "cbc" ["out.lp"] ""
  let obj = read $ last $ splitOn " " $ head $ filter (isPrefixOf "Objective value: ") $ lines out :: Double
  return (round obj)

main19 :: IO ()
main19 = do
    input <- parseBluePrints . lines <$> readFile "res/input19"
    objs <- mapM (solveOne 24) input
    print $ sum $ zipWith (*) [1..] objs
    objsB <- mapM (solveOne 32) (take 3 input) 
    print $ product objsB

module Perfect.Wall (primes, bricks, wall, bestPrimeDivisor) where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Data.Numbers.Primes as Primes
import Control.Arrow
import Data.Ord
import Data.Ratio
import Data.List
import Math.NumberTheory.Primes.Factorisation

import Perfect.Config (maxPrime, maxPower, sigmaPrimorial)
import Perfect.Types (FactRat, (%%), (**), numerFactors, eq1, numerEq1, numerCoprime)

ourFactorise = factorise'

primes :: [Int]
primes = map fromInteger $ takeWhile (<= maxPrime) Primes.primes

ratios :: Int -> [(FactRat, Integer)]
ratios p' = map (\(sigma, pa) -> (sigma %% pa, pa)) ratios' where
  p = fromIntegral p'
  -- Initial set of sigma on primorials
  ratios'' = [ (sigmaPrimorial p a, p^a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Thus such elements should be removed
  ratios' = filter (\(a,_) -> pred a) ratios'' where
    pred 1 = True
    pred n
      | gcd prod n == 1 = False
      | otherwise = pred (n `div` gcd prod n)
    prod = product (map fromIntegral primes)


bricks :: Map.IntMap [(FactRat, Integer)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength :: Map.IntMap Int
bricksLength = Map.map length bricks

bestPrimeDivisor :: [(Int, Int)] -> (Int, Int)
bestPrimeDivisor = minimumBy (comparing f) where
  f p = length $ (Map.!) bricks (fst p)

wall :: Map.IntMap [(FactRat, Integer)] -> [Int] -> [(FactRat, Integer)] -> [(FactRat, Integer)]
wall brs prs = wall' where
  wall' = concatMap f where
    f (ratio, n)
      | eq1 ratio = [(ratio, n)]
      | numerEq1 ratio = []
      | not (numerCoprime ratio n) = []
      | otherwise = wall' [(ratio**rat, n*pa) | (rat,pa)<-pile] where
        pile = dropWhile (\(_,pa) -> pa < (fromIntegral p)^a) ((Map.!) brs p)
        (p, a) = bestPrimeDivisor $ numerFactors ratio

sigmaN :: Integer -> Integer
sigmaN n = product $ map (\(p,a)->sigmaPrimorial p (fromIntegral a)) (ourFactorise n)

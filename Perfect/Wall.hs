module Perfect.Wall (ratios, primes, bricks, wall, bestPrimeDivisor) where

import Prelude hiding ((**))
import Data.Ord
import Data.List
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Math.NumberTheory.Primes.Sieve as Primes

import Perfect.Config (maxPrime, maxPower, sigmaPrimorial)
import Perfect.Types (FactorizedRatio, FactRat, (%%), (**), numerFactors, eq1, numerEq1, numerCoprime)

primes :: [Int]
primes = map fromInteger $ takeWhile (<= maxPrime) Primes.primes

ratios :: FactorizedRatio a => Int -> [(a, Integer)]
ratios p' = map (\(sigma, pa) -> (sigma %% pa, pa)) ratios' where
  p :: Integer
  p = fromIntegral p'
  -- Initial set of sigma on primorials
  ratios'' :: [(Integer, Integer)]
  ratios'' = [ (sigmaPrimorial p a, p^a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Such elements can be safely removed
  ratios' :: [(Integer, Integer)]
  ratios' = filter (predicate . fst) ratios'' where
    predicate n
      | n==1 = True
      | gcd prod n == 1 = False
      | otherwise = predicate (n `div` gcd prod n)
    prod = product (map fromIntegral primes)


bricks :: Map.IntMap [(FactRat, Integer)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength :: Map.IntMap Int
bricksLength = Map.map length bricks

bestPrimeDivisor :: [(Int, Int)] -> (Int, Int)
bestPrimeDivisor = minimumBy (comparing f) {-. filter (g . fst)-} where
  f p = (Map.!) bricksLength (fst p)
  --g p = tryNumber`mod`p /= 0

wall :: [(FactRat, Integer)] -> [(FactRat, Integer)]
wall = concatMap f where
  f (ratio, n)
    | eq1 ratio = [(ratio, n)]
    | numerEq1 ratio = []
    | not (numerCoprime ratio n) = []
    | otherwise = wall [(ratio**rat, n*pa) | (rat,pa)<-pile] where
      pile = dropWhile (\(_,pa) -> pa < (fromIntegral p)^a) ((Map.!) bricks p)
      (p, a) = bestPrimeDivisor $ numerFactors ratio

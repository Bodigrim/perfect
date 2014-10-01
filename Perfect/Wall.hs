module Perfect.Wall (primes, bricks, wall) where

import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Data.Numbers.Primes as Primes
import Control.Arrow
import Data.Ord
import Data.Ratio
import Data.List
import Math.NumberTheory.Primes.Factorisation

import Perfect.Config (maxPrime, maxPower, sigmaPrimorial)

ourFactorise = factorise'
--ourFactorise = trialDivisionTo maxPrime
--ourFactorSieve = factorSieve maxPrime
--ourFactorise = sieveFactor ourFactorSieve

primes :: [Int]
primes = map fromInteger $ takeWhile (<= maxPrime) Primes.primes

ratios :: Int -> [(Ratio Integer, Integer)]
ratios p' = map (\(sigma, pa) -> (sigma % pa, pa)) ratios' where
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

bricks :: Map.IntMap [(Ratio Integer, Integer)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength = Map.map length bricks

bestPrimeDivisor :: Integer -> Int
bestPrimeDivisor n = minimumBy cmp (map (fromInteger . fst) $ ourFactorise n) where
  cmp = comparing (\p -> length $ (Map.!) bricks p)

wall :: Map.IntMap [(Ratio Integer, Integer)] -> [Int] -> [(Ratio Integer, Integer)] -> [(Ratio Integer, Integer)]
wall brs prs = wall' where
  wall' = concatMap f where
    f (ratio, n)
      | ratio==1 = [(ratio, n)]
      | num==1 = []
      | gcd num n /= 1 = []
      | otherwise = wall' [(ratio*rat, n*pa) | (rat,pa)<-pile] where
        num = numerator ratio
        pile = dropWhile (\(_,pa) -> num `div` (fromIntegral p) `mod` pa==0) ((Map.!) brs p)
        p = bestPrimeDivisor num

sigmaN :: Integer -> Integer
sigmaN n = product $ map (\(p,a)->sigmaPrimorial p (fromIntegral a)) (ourFactorise n)

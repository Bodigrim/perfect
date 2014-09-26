module Perfect.Wall (primes, bricks, wall) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

primes :: [Integer]
primes = takeWhile (<= maxPrime) Primes.primes

ratios :: Integer -> [(Ratio Integer, Integer)]
ratios p = map (\(sigma, pa) -> (sigma % pa, pa)) ratios' where
  -- Initial set of sigma on primorials
  ratios'' = [ (sigmaPrimorial p a, p^a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Thus such elements should be removed
  ratios' = filter (\(a,_) -> pred a) ratios'' where
    pred 1 = True
    pred n
      | gcd prod n == 1 = False
      | otherwise = pred (n `div` gcd prod n)
    prod = product primes

bricks :: Map.Map Integer [(Ratio Integer, Integer)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength = Map.map length bricks

wall brs prs = wall' where
  wall' = concatMap f where
    bestPrimeDivisor n = minimumBy cmp (map fst $ ourFactorise n) where
      cmp = comparing (\p -> length $ (Map.!) brs p)
    f (ratio, n)
      | ratio==1 = [(ratio, n)]
      | num==1 = []
      -- | numerator perfectness `mod` gcd num n == 0 = []
      | gcd num n /= 1 = []
      | p==0 = []
      | otherwise = wall' [(ratio*rat, n*pa) | (rat,pa)<-pile] where
        num = numerator ratio
        --den = denominator ratio
        pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) pileFull
        pileFull = (Map.!) brs p
        p = bestPrimeDivisor num

sigmaN n = product $ map (\(p,a)->sigmaPrimorial p (fromIntegral a)) (ourFactorise n)

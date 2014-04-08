module Perfect.Wall where

import qualified Data.Map.Strict as Map
import qualified Data.Numbers.Primes as Primes
import Control.Arrow
import Data.Ord
import Data.Ratio
import Data.List

primesGen :: Integer -> [Integer]
primesGen maxPrime = takeWhile (<= maxPrime) Primes.primes

ratiosGen :: (Integer -> Integer -> Integer) -> [Integer] -> Integer -> [[(Ratio Integer, Integer)]]
ratiosGen sigmaPrimorial primes maxPower = map ( map (\(sigmame, pa) -> (sigmame % pa, pa)) ) ratios' where
  -- Initial set of sigma on primorials
  ratios'' = map (\p -> map (sigmaPrimorial p &&& (^) p) [1..maxPower]) primes
  -- If numerator contains primes > maxPrime it cannot be cancelled out
  ratios' = map (filter (\(a,_) -> pred a)) ratios'' where
    pred 1 = True
    pred n | gcd prod n == 1 = False
           | otherwise = pred (n `div` gcd prod n)
    prod = product primes

wall perfectness brs prs = wall' where
  wall' = concatMap f where
    prsSorted = sortBy cmp prs where
      cmp = comparing (\p -> (length $ (Map.!) brs p, p))
    bestPrimeDivisor n = head $ filter (\p -> n`mod`p==0) prsSorted
    f (ratio, n)
      | ratio==perfectness = [(ratio, n)]
      | gcd num n /= 1 = []
      | otherwise = wall' [(ratio*rat, n*pa) | (rat,pa)<-pile] where
        num = numerator ratio
        pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) pileFull
        pileFull = (Map.!) brs p
        p = bestPrimeDivisor num

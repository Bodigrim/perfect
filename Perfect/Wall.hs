module Perfect.Wall where

import qualified Data.Map.Strict as Map
import qualified Data.Numbers.Primes as Primes
import Control.Arrow
import Data.Ord
import Data.Ratio
import Data.List
import Math.NumberTheory.Primes.Factorisation

import Perfect.Config

primes :: [Integer]
primes = takeWhile (<= maxPrime) Primes.primes

ratios :: [[(Ratio Integer, Integer)]]
ratios = map ( map (\(sigmame, pa) -> (sigmame % pa, pa)) ) ratios' where
  -- Initial set of sigma on primorials
  ratios'' = map (\p -> {-filter (\(_,pa) -> pa <= maxPrimorial) $-} map (sigmaPrimorial p &&& (^) p) [1..maxPower]) primes
  -- If numerator contains primes > maxPrime it cannot be cancelled out
  ratios' = map (filter (\(a,_) -> pred a)) {- $ map (filter (\(a,_) -> a/=1)) -} ratios'' where
    pred 1 = True
    pred n
      | gcd prod n == 1 = False
      | otherwise = pred (n `div` gcd prod n)
    prod = product primes

bricks :: Map.Map Integer [(Ratio Integer, Integer)]
bricks = Map.fromList $ zip primes ratios

perf = 1%1

wall brs prs = wall' where
  wall' = concatMap f where
    prsSorted = sortBy cmp prs where
      cmp = comparing (\p -> (length $ (Map.!) brs p, p))
    bestPrimeDivisor n = head $ filter (\p -> n`mod`p==0) prsSorted ++ [0]
    f (ratio, n)
      | ratio==1 = [(ratio, n)]
      -- | isSimpleProd (1/ratio) && gcd n num == 1 = [(1, n*num)]
      | num==1 = []
      -- | numerator perfectness `mod` gcd num n == 0 = []
      | gcd num n /= 1 = []
      | p==0 = []
      | otherwise = wall' [(ratio*rat, n*pa) | (rat,pa)<-pile] where
        num = numerator ratio
        --den = denominator ratio
        pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) pileFull
        pileFull = (Map.!) brs p
        p = bestPrimeDivisor (num `div` gcd num (numerator perf))


isSimpleProd ratio = sigmaN den == num where
  num = numerator ratio
  den = denominator ratio

sigmaN n = product $ map (\(p,a)->sigmaPrimorial p (fromIntegral a)) (factorise' n)

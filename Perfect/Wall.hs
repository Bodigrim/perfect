{-# LANGUAGE BangPatterns #-}

module Perfect.Wall (ratios, primes, bricks, wall, bestPrimeDivisor) where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Math.NumberTheory.Primes.Sieve as Primes

import Perfect.Config (maxPrime, maxPower, sigmaPrimorial)
import Perfect.Types (FactRat, (%%), (**), numerFactors, eq1, numerEq1, numerCoprime)

primes :: [Int]
primes = map fromInteger $ takeWhile (<= maxPrime) Primes.primes

ratios :: Int -> [(FactRat, Integer)]
ratios p' = map (\(sigma, pa) -> (sigma %% pa, pa)) ratios' where
  p :: Integer
  p = fromIntegral p'
  -- Initial set of sigma on primorials
  ratios'' :: [(Integer, Integer)]
  ratios'' = [ (sigmaPrimorial p a, p^a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Such elements can be safely removed
  ratios' :: [(Integer, Integer)]
  ratios' = filter (predicate prod . fst) ratios'' where
    predicate _ 1 = True
    predicate 1 _ = False
    predicate m n = (d == n) || predicate d (n `div` d)
      where
        d = gcd m n
  prod :: Integer
  prod = product (map fromIntegral primes)


bricks :: Map.IntMap [(FactRat, Integer)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength :: Map.IntMap Int
bricksLength = Map.map length bricks

bestPrimeDivisor :: [(Int, Int)] -> (Int, Int)
bestPrimeDivisor [] = error "bestPrimeDivisor: empty list"
bestPrimeDivisor (h@(p,_):xs) = if lenAtP == 1 then h else go (lenAtP, h) xs
  where
    lenAtP = (Map.!) bricksLength p
    go (_, prevRet) [] = prevRet
    go prev@(prevLen, _) (new@(pp,_):ys) = if lenAtPP == 1 then new
      else go (if lenAtPP < prevLen then (lenAtPP, new) else prev) ys
      where
        lenAtPP = (Map.!) bricksLength pp

wall :: (FactRat, Integer) -> [Integer]
wall (!ratio, !n)
  | eq1 ratio = [n]
  | numerEq1 ratio = []
  | not (numerCoprime ratio n) = []
  | otherwise = concatMap wall [(ratio**rat, n*pa) | (rat,pa)<-pile] where
    pile = dropWhile (\(_,pa) -> pa < fromIntegral p ^ a) ((Map.!) bricks p)
    (p, a) = bestPrimeDivisor $ numerFactors ratio

{-# LANGUAGE BangPatterns #-}

module Perfect.Wall (ratios, primes, bricks, wall, bestPrimeDivisor) where

import Prelude hiding ((**))
import Control.Arrow
import qualified Data.IntMap.Strict as Map
import qualified Math.NumberTheory.Primes.Sieve as Primes
import Numeric.Natural
import GHC.Natural

import Perfect.Config (maxPrime, maxPower, sigmaPrimorial)
import Perfect.Types

primes :: [Word]
primes = map fromIntegral $ takeWhile (<= fromIntegral maxPrime) Primes.primes

ratios :: Word -> [(FactRat, Natural)]
ratios p' = map (\(sigma, pa) -> (sigma %% pa, pa)) ratios' where
  p :: Natural
  p = wordToNatural p'
  -- Initial set of sigma on primorials
  ratios'' :: [(Natural, Natural)]
  ratios'' = [ (sigmaPrimorial p a, p^a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Such elements can be safely removed
  ratios' :: [(Natural, Natural)]
  ratios' = filter (predicate prod . fst) ratios'' where
    predicate _ 1 = True
    predicate 1 _ = False
    predicate m n = (d == n) || predicate d (n `div` d)
      where
        d = gcd m n
  prod :: Natural
  prod = product (map wordToNatural primes)


bricks :: Map.IntMap [(FactRat, Natural)]
bricks = Map.fromAscList $ map (fromIntegral &&& ratios) primes

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

wall :: (FactRat, Natural) -> [Natural]
wall (!ratio, !n)
  | eq1 ratio = [n]
  | numerEq1 ratio = []
  | not (numerCoprime ratio n) = []
  | otherwise = concatMap wall [(ratio**rat, n*pa) | (rat,pa)<-pile] where
    pile = dropWhile (\(_,pa) -> pa < intToNatural p ^ a) ((Map.!) bricks p)
    (p, a) = bestPrimeDivisor $ numerFactors ratio

module Perfect.Wall (ratios, primes, bricks, wall, bestPrimeDivisor) where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Monoid
import qualified Math.NumberTheory.Primes.Sieve as Primes

import Perfect.Config
import Perfect.Types

primes :: [Int]
primes = map fromInteger $ takeWhile (<= maxPrime) Primes.primes

ratios :: Int -> [(FactRat, Int)]
ratios p' = map (\(sigma, pa, a) -> (sigma %% pa, a)) ratios' where
  p :: Integer
  p = fromIntegral p'
  -- Initial set of sigma on primorials
  ratios'' :: [(Integer, Integer, Int)]
  ratios'' = [ (runSigmaF sigmaPrimorial p (fromIntegral a), p ^ fromIntegral a, a) | a <- [1..maxPower] ]
  -- If sigma on primorials contains primes > maxPrime it cannot be cancelled out
  -- Such elements can be safely removed
  ratios' :: [(Integer, Integer, Int)]
  ratios' = filter (\(v, _, _) -> predicate prod v) ratios'' where
    predicate _ 1 = True
    predicate 1 _ = False
    predicate m n = (d == n) || predicate d (n `div` d)
      where
        d = gcd m n
  prod :: Integer
  prod = product (map fromIntegral primes)


bricks :: Map.IntMap [(FactRat, Int)]
bricks = Map.fromSet ratios (Set.fromList primes)

bricksLength :: Map.IntMap Int
bricksLength = Map.map length bricks

data Min1 = Min1 !Int !Int deriving (Eq, Ord, Show)

instance Monoid Min1 where
  mempty = Min1 minBound maxBound
  a@(Min1 _ 1) `mappend` _ = a
  _ `mappend` b@(Min1 _ 1) = b
  a@(Min1 _ i) `mappend` b@(Min1 _ j)
    | i <= j = a
    | otherwise = b

bestPrimeDivisor :: FactRat -> Int
bestPrimeDivisor ps = x
  where
    Min1 x _ = Map.foldlWithKey f mempty ps
    f acc p a = acc <> (if a > 0 then Min1 p ((Map.!) bricksLength p) else mempty)

wall :: FactRat -> FactRat -> [Integer]
wall ratio n
  | not (numerCoprime ratio n) = []
  | eq1 ratio = [fst $ numDen n]
  | numerEq1 ratio = []
  | otherwise = concat [(wall $ ratio ** rat) $ Map.insert p a n | (rat, a) <- pile]
    where
      pile = dropWhile ((< aMin) . snd) ((Map.!) bricks p)
      aMin = (Map.!) ratio p
      p = bestPrimeDivisor ratio

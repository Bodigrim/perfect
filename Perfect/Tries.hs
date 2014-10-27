module Perfect.Tries where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Perfect.Types (FactRat, (%%), (**), (\\))
import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: [(FactRat, Integer)] -> [Either (FactRat, Integer) Integer]
tryGen start = concat (map mapper start `using` parList rseq) where
		mapper :: (FactRat, Integer) -> [Either (FactRat, Integer) Integer]
		mapper x = Right (snd x) : map Left (wall [x])

tryN :: Int -> [Either (FactRat, Integer) Integer]
tryN n = tryGen (start n) where
  start 1 = [((1%%1)\\perfectness, 1)]
  start m = [ (rat1**rat2, pa1*pa2) | (rat2, pa2) <- (Map.!) bricks p, (rat1, pa1) <- start (m`div`p)] where
    p = last $ filter (\pr -> m`mod`pr==0) primes

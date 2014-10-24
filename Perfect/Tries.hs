module Perfect.Tries where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Perfect.Types (FactRat, (%%), (**), (\\))
import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: Map.IntMap [(FactRat, Integer)] -> [Int] -> [(FactRat, Integer)] -> [Either (FactRat, Integer) Integer]
tryGen brs prs start = concat (map mapper start `using` parList rseq) where
		mapper :: (FactRat, Integer) -> [Either (FactRat, Integer) Integer]
		mapper x = Right (snd x) : map Left (wall brs prs [x])

tryN :: Int -> [Either (FactRat, Integer) Integer]
tryN n = tryGen brs prs (start n) where
  start 1 = [((1%%1)\\perfectness, 1)]
  start n = [ (rat1**rat2, pa1*pa2) | (rat2, pa2) <- (Map.!) bricks p, (rat1, pa1) <- start (n`div`p)] where
    p = last $ filter (\p -> n`mod`p==0) primes
  prs = filter (\p -> n`mod`p/=0) primes
  brs = Map.filterWithKey (\p -> \_ -> n`mod`p/=0)  bricks

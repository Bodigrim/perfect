module Perfect.Tries where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Perfect.Types (FactRat, (%%), (**), (\\))
import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: [(FactRat, Integer)] -> [Either Integer Integer]
tryGen start = concat (map mapper start `using` parList rseq) where
  mapper :: (FactRat, Integer) -> [Either Integer Integer]
  mapper x = map Left (wall x) ++ [Right (snd x)]

tryN :: Int -> [Integer] -> [Either Integer Integer]
tryN n alreadyProcessed = tryGen $ skip $ start n where
  skip = filter ((`notElem` alreadyProcessed) . snd)

  start 1 = [((1%%1)\\perfectness, 1)]
  start m = [ (rat1**rat2, pa1*pa2) | (rat2, pa2) <- (Map.!) bricks p, (rat1, pa1) <- start (m`div`p)] where
    p = last $ filter (\pr -> m`mod`pr==0) primes

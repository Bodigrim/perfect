module Perfect.Tries where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies
import Numeric.Natural

import Perfect.Types (FactRat, (%%), (**), (\\))
import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: [(FactRat, Natural)] -> [Either Natural Natural]
tryGen start = concat (map mapper start `using` parListChunk 1 rdeepseq) where
  mapper :: (FactRat, Natural) -> [Either Natural Natural]
  mapper x = map Left (wall x) ++ [Right (snd x)]

tryN :: Word -> [Natural] -> [Either Natural Natural]
tryN n alreadyProcessed = tryGen $ skip $ start n where
  skip = filter ((`notElem` alreadyProcessed) . snd)

  start :: Word -> [(FactRat, Natural)]
  start 1 = [((1%%1)\\perfectness, 1)]
  start m = [ (rat1**rat2, pa1*pa2) | (rat2, pa2) <- (Map.!) bricks (fromIntegral p), (rat1, pa1) <- start (m`div`p)] where
    p = last $ filter (\pr -> m`mod`pr==0) primes

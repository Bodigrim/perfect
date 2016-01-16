module Perfect.Tries where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Perfect.Types
import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: [(FactRat, FactRat)] -> [Either Integer Integer]
tryGen seeds = concat (map mapper seeds `using` parListChunk 1 rdeepseq)
  where
    mapper (seedRatio, seed)
      = map Left (wall seedRatio seed)
      ++ [Right (fst $ numDen seed)]

tryN :: Int -> [Integer] -> [Either Integer Integer]
tryN n alreadyProcessed = tryGen $ skip $ start n where
  skip = filter ((`notElem` alreadyProcessed) . fst . numDen . snd)

  start :: Int -> [(FactRat, FactRat)]
  start 1 = [(mempty \\ perfectness, mempty)]
  start m = [ (rat1 ** rat2, Map.insert p a2 pa1) | (rat2, a2) <- (Map.!) bricks p, (rat1, pa1) <- start (m `div` p)]
    where
      p = last $ filter (\pr -> m `mod` pr == 0) primes

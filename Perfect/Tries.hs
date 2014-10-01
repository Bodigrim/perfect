module Perfect.Tries where

import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies
import Data.Ratio
import Control.DeepSeq

import Perfect.Wall (primes, bricks, wall)
import Perfect.Config (perfectness)

tryGen :: Map.IntMap [(Ratio Integer, Integer)] -> [Int] -> [(Ratio Integer, Integer)] -> [(Ratio Integer, Integer)]
tryGen brs prs start = concat (map (\x -> printer x ++ wall brs prs [x]) start `using` parList rseq)

tryN :: Int -> [(Ratio Integer, Integer)]
tryN n = tryGen brs prs (start n) where
  start 1 = [(1/perfectness, 1)]
  start n = [ (rat1*rat2, pa1*pa2) | (rat2, pa2) <- (Map.!) bricks p, (rat1, pa1) <- start (n`div`p)] where
    p = last $ filter (\p -> n`mod`p==0) primes
  prs = filter (\p -> n`mod`p/=0) primes
  brs = Map.filterWithKey (\p -> \_ -> n`mod`p/=0)  bricks

printer (a, b) = if b`mod`49==0 then [] else [(-1%1, b)]

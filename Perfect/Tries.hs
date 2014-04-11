module Perfect.Tries where

import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Ratio
import Control.DeepSeq

import Perfect.Wall
import Perfect.Config

tryGen brs prs start = concat (map (\x -> printer x ++ wall brs prs [x]) start `using` parList rseq)

tryN n = tryGen brs prs (start n) where
  start 1 = [(1/perfectness, 1)]
  start n = [ (rat1*rat2, pa1*pa2) | (rat1, pa1) <- start (n`div`p), (rat2, pa2) <- (Map.!) bricks p] where
    p = last $ filter (\p -> n`mod`p==0) primes
  prs = filter (\p -> n`mod`p/=0) primes
  brs = Map.filterWithKey (\p -> \_ -> n`mod`p/=0)  bricks

printer (a, b) = if b`mod`9==0 || b`mod`25==0 then [] else [(-1%1, b)]

module Perfect.Tries where

import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Ratio
import Control.DeepSeq

import Perfect.Wall
import Perfect.Config

tryGen brs prs start = concat (map (\x -> printer x ++ wall brs prs [x]) start `using` parList rseq)

try30 = tryGen bricks30 primes30 start30 where
  start30 = [ (rat2*rat3*rat5/perfectness, pa2*pa3*pa5) |  (rat2, pa2) <- (Map.!) bricks 2, (rat3, pa3) <- (Map.!) bricks 3, (rat5, pa5) <- (Map.!) bricks 5]
  primes30 = dropWhile (<=5) primes
  bricks30 = Map.filterWithKey (\k -> \_ -> k>5)  bricks

try6 = tryGen bricks6 primes6 start6 where
  start6 = [ (rat2*rat3/perfectness, pa2*pa3) |  (rat2, pa2) <- (Map.!) bricks 2, (rat3, pa3) <- (Map.!) bricks 3]
  primes6 = dropWhile (<=3) primes
  bricks6 = Map.filterWithKey (\k -> \_ -> k>3) bricks

try2 = concat (map (\x -> printer x ++ wall bricks2 primes2 [x]) start `using` parList rseq) where
  start = [ (rat2/perfectness, pa2) |  (rat2, pa2) <- (Map.!) bricks 2]
  primes2 = dropWhile (<=2) primes
  bricks2 = Map.filterWithKey (\k -> \_ -> k>2)  bricks

tryN 2 = try2
tryN 6 = try6
tryN 30 = try30
tryN _ = undefined

printer (a, b) = if b`mod`9==0 then [] else [(-1%1, b)]

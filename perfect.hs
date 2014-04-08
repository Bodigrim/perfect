import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Ratio
import Control.DeepSeq

import Perfect.Sigmas
import Perfect.Wall

sigmaPrimorial = sigmaModExpPrimorial

main =
  --mapM (\(_,a) -> appendFile "e-perfect.txt" (show a ++ "\n")) try
  mapM (\(_,a) -> print a) try6

maxPrime = 700 :: Integer
maxPower = 70  :: Integer
perfectness = 2%1 :: Rational


primes = primesGen maxPrime
ratios = primes `deepseq` ratiosGen sigmaPrimorial primes maxPower

bricks :: Map.Map Integer [(Ratio Integer, Integer)]
bricks = ratios `deepseq` Map.fromList $ zip primes ratios

primes2 = primes `deepseq` dropWhile (<=2) primes
bricks2 = bricks `deepseq` Map.delete 2 bricks

primes6 = primes `deepseq`dropWhile (<=3) primes
bricks6 = bricks `deepseq` Map.delete 2 $ Map.delete 3 bricks



try6 :: [(Ratio Integer, Integer)]
try6 = concat (map (\x -> wall perfectness bricks6 primes6 [x]) start `using` parList rseq) where
  start = [ (rat2*rat3, pa2*pa3) |  (rat2, pa2) <- (Map.!) bricks 2, (rat3, pa3) <- (Map.!) bricks 3]

try2 :: [(Ratio Integer, Integer)]
try2 = concat (map (\x -> wall perfectness bricks2 primes2 [x]) start `using` parList rseq) where
  start = (Map.!) bricks 2

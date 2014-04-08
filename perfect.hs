import qualified Data.Numbers.Primes as Primes
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Data.List
import Control.Parallel.Strategies
import Data.Ratio
import qualified Control.Arrow
import Data.Ord

sigmaPrimorial = sigmaModExpPrimorial

main =
  --mapM (\(_,a) -> appendFile "e-perfect.txt" (show a ++ "\n")) try
  mapM (\(_,a) -> print a) try

maxPrime = 2000 :: Integer
maxPower = 200

perfectness :: Rational
perfectness = 2%1 -- for perfect
--perfectness = 33%16 -- for multiperfect

primes = takeWhile (<= maxPrime) Primes.primes

ratios = map (\p -> map (sigmaPrimorial p Control.Arrow.&&& (^) p) [1..maxPower]) primes

-- If numerator contains primes > maxPrime it cannot be cancelled out
ratios' = map (filter (\(a,_) -> pred a)) ratios where
  pred 1 = True
  pred n | gcd prod n == 1 = False
         | otherwise = pred (n `div` gcd prod n)
  prod = product primes

ratios1 = map (filter (\(a,_) -> pred a)) ratios where
  pred a = a == gcd a prod
  prod = product $ map ( snd . last) ratios'

ratios2 = map ( map (\(sigmame, pa) -> (sigmame % pa, pa)) ) ratios1

bricks :: Map.Map Integer [(Ratio Integer, Integer)]
bricks = Map.fromList $ zip primes ratios2

primes5 = dropWhile (<5) primes
bricks5 = Map.delete 2 $ Map.delete 3 bricks where
  bricks' = Map.fromList $ zip primes (map tail $ filter (\xs -> length xs > 1) ratios2)

primes5' = sortBy cmp primes5 where
  cmp = comparing (\p -> (length $ (Map.!) bricks p, p))

wall :: [(Rational, Integer)] -> [(Rational, Integer)]
wall = concatMap f where
  f (ratio, n)
    | ratio==perfectness = [(ratio, n)]
    | gcd num n /= 1 = []   -- n`mod`p==0 = []
    | otherwise = wall [(ratio*rat, n*pa) | (rat,pa)<-pile] where
      num = numerator ratio
      pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) pileFull
      --pileFull = ((p+1) % p, p) : (Map.findWithDefault [] p bricks5)
      pileFull = (Map.!) bricks5 p
      p = minPrimeDivisor5 num

wall' = concatMap f where
  f (ratio, n)
    | ratio==perfectness = [(ratio, n)]
    | gcd num n /= 1 = []   -- n`mod`p==0 = []
    | otherwise = wall [(ratio*rat, n*pa) | (rat,pa)<-pile] where
      num = numerator ratio
      pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) pileFull
      pileFull = (Map.!) bricks p
      p = minPrimeDivisor num

minPrimeDivisor n = head $ filter (\p -> n`mod`p==0) primes
minPrimeDivisor5 n = head $ filter (\p -> n`mod`p==0) primes5'

maxPrimeDivisor n = helper n primes where
  helper n [] = n
  helper n (p:ps) | n`mod`p==0 = if helper2 n p == 1 then p else helper (helper2 n p) ps
                  | otherwise = helper n ps

  helper2 n p | n`mod`p==0 = helper2 (n`div`p) p
              | otherwise = n


try = concat (map (\x -> wall [x]) start `using` parList rseq) where
  start = [ (rat2*rat3, pa2*pa3) |  (rat2, pa2) <- (Map.!) bricks 2, (rat3, pa3) <- (Map.!) bricks 3]

try' = concat (map (\x -> wall' [x]) start `using` parList rseq) where
  start = (Map.!) bricks 2

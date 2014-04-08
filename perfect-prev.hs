import qualified Data.Numbers.Primes as Primes
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Data.List
import Control.Parallel.Strategies
import Data.Ratio
import qualified Control.Arrow

sigmaPrimorial = sigmamePrimorial

main =
  --mapM (\(_,a) -> appendFile "e-perfect.txt" (show a ++ "\n")) try
  mapM (\(_,a) -> print a) try

maxPrime = 700 :: Integer
maxPower = 70

perfectness :: Integer
perfectness = 2 -- for perfect
--perfectness = 3 -- for multiperfect

primes = takeWhile (<= maxPrime) Primes.primes

sigmamePrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmauPrimorial p a = if a==0 then 1 else p^a + 1

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

wall :: [(Rational, Integer)] -> [(Rational, Integer)]
wall xs = concatMap f xs  `using` parList rseq where
  f (ratio, n)
    | ratio==perfectness%1 = [(ratio, n)]
    | gcd num n /= 1 = []   -- n`mod`p==0 = []
    | otherwise = wall [(ratio*rat, n*pa) | (rat,pa)<-pile] where
      num = numerator ratio
      pile = dropWhile (\(_,pa) -> num`div`p`mod`pa==0) $ (Map.!) bricks p
      p = minPrimeDivisor num

minPrimeDivisor n = head $ filter (\p -> n`mod`p==0) primes

maxPrimeDivisor n = helper n primes where
  helper n [] = n
  helper n (p:ps) | n`mod`p==0 = if helper2 n p == 1 then p else helper (helper2 n p) ps
                  | otherwise = helper n ps

  helper2 n p | n`mod`p==0 = helper2 (n`div`p) p
              | otherwise = n


try = wall ((Map.!) bricks 2)


--wall xs = concatMap f xs where
--  f (ratio, n)
--    | ratio==perfectness%1 = [(ratio, n)]
--    | n`mod`p==0 = []
--    | otherwise = wall [(ratio*rat, n*pa) | (rat,pa)<-pile, ratio*rat<=perfectness%1]  where
--      num = numerator ratio
--      den = denominator ratio
--      pile = filter pred $ dropWhile (\(_,pa) -> num`div`p`mod`pa==0) $ (Map.!) bricks p
--      pred (rat, pa) = gcd (num3 `div` gcd num3 den) den == 1 where
--        num2 = numerator rat
--        num3 = if num2`mod`perfectness == 0 then num2`div`perfectness else num2
--      p = minPrimeDivisor num

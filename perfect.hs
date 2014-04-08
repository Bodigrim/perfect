import qualified Data.Numbers.Primes as Primes
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Data.List
import Control.Parallel.Strategies
import Data.Ratio
import qualified Control.Arrow
import Data.Ord

sigmaPrimorial = sigmaModExpPrimorial

{-
https://oeis.org/A160678
https://oeis.org/A127724
https://oeis.org/A072002

https://oeis.org/A091321 - !!! 162729000
https://oeis.org/A092356 - !!! 920387520 203558400...
https://oeis.org/A092760
https://oeis.org/A066218
https://oeis.org/A007358
A092788
https://oeis.org/A066226
-}

main =
  --mapM (\(_,a) -> appendFile "e-perfect.txt" (show a ++ "\n")) try
  mapM (\(_,a) -> print a) try

maxPrime = 2000 :: Integer
maxPower = 200

perfectness :: Rational
perfectness = 2%1 -- for perfect
--perfectness = 33%16 -- for multiperfect

primes = takeWhile (<= maxPrime) Primes.primes

sigmaUsualPrimorial p a = sum [ p^b | b<-[0..a]]
sigmaAlterPrimorial p a = sum [ p^b * (-1)^(a-b) | b<-[0..a]]
sigmaModExpPrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmaUnitPrimorial p a = if a==0 then 1 else p^a + 1
sigmaNonUnitPrimorial p a = sigmaUsualPrimorial p a - sigmaUnitPrimorial p a

sigmaOrdUnitPrimorial 2 = sigmaUsualPrimorial 2
sigmaOrdUnitPrimorial p = sigmaUnitPrimorial p

sigmaUnitOrdPrimorial 2 = sigmaUnitPrimorial 2
sigmaUnitOrdPrimorial p = sigmaUsualPrimorial p

--a066218Primorial p a =

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

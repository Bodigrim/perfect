import qualified Data.Numbers.Primes as Primes
import qualified Data.Map.Strict as Map
import Control.DeepSeq

import Data.Ratio

--newtype Ratio = Ratio (Integer, Integer, Float)

--(%) :: Integer -> Integer -> Ratio
--a % b = Ratio (a, b, fromInteger a / fromInteger b)

--numerator :: Ratio -> Integer
--numerator (Ratio (x,_,_)) = x

--instance Num Ratio where
--	(Ratio (a,b,c)) * (Ratio (d,e,f)) = Ratio ( (a`div`ae) * (d`div`bd), (b`div`bd) * (e`div`ae), c*f) where
--		ae = gcd a e
--		bd = gcd b d
--	fromInteger a = Ratio (a, 1, fromInteger a)

--instance Eq Ratio where
--	(Ratio (a,b,c)) == (Ratio (d,e,f)) = a==d && b==e

--instance Ord Ratio where
--	(Ratio (a,b,c)) <= (Ratio (d,e,f)) = c <= f
--	(Ratio (a,b,c)) < (Ratio (d,e,f)) = c < f

sigmaPrimorial = sigmauPrimorial

main = do
	mapM (\(_,a,_) -> appendFile "u-perfect.txt" (show a ++ "\n")) try
	--mapM (\(_,a,_) -> putStrLn (show a)) try

maxPrime = 1000 :: Integer
maxPower = 50

perfectness = 2 -- for perfect
--perfectness = 3 -- for multiperfect

primes = takeWhile (<= maxPrime) Primes.primes

sigmamePrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmauPrimorial p a = if a==0 then 1 else p^a + 1

-- Every e-perfect number must be divisible by 6
ratios = map (\p -> map (\a -> (sigmaPrimorial p a, p^a, p)) [(if p <= 3 then 1 else 0)..maxPower]) primes

-- If numerator contains primes > maxPrime it cannot be cancelled out
ratios' = map (filter (\(a,_,_) -> pred a)) ratios where
	pred 1 = True
	pred n | gcd prod n == 1 = False
	       | otherwise = pred (n `div` gcd prod n)
	prod = product primes

--ratios1 = ratios'

ratios1 = map (filter (\(a,_,_) -> pred a)) ratios where
	pred a = a == gcd a prod
	prod = product $ map ( (\(_,pa,_)->pa) . last) ratios'

ratios2 = reverse ratios1

-- convert triples to ratio, number and maximal prime factor
ratios3 = map (\xs -> map (\(sigmame, pa, p) -> (sigmame%pa, pa, p)) xs) ratios2

--maxMults :: [Double]
maxMults :: [Rational]
maxMults = map fromRational $ scanl (*) 1 $ reverse ps where
	ps = map (\p -> p % sigmaPrimorial p 1) primes

maxMultMap = Map.fromList $ zip (reverse primes) maxMults

primorials = tail $ scanl (*) 1 primes

primorialMap = Map.fromList $ zip primes primorials

supPrimorials = tail $ reverse $ scanl (*) 1 $ reverse $ map ( (\(_,pa,_)->pa) . last) ratios1

supPrimorialMap = Map.fromList $ zip primes supPrimorials

-- combinations combines elements together
-- and use pred for early return
combinations :: (b->Bool) -> (a -> b -> b) -> b -> [[a]] -> [b]
combinations pred f acc ys = foldr g [acc] ys where
	g xs zs = filter pred $ concatMap (\z -> map (\x -> x`f`z) xs) zs

simpleCombinations = combinations (\_ -> True) (:) []

try = combinations pred f (1 % perfectness,1,1) ratios3 where
	pred (a, _, p) = (a <= 1) && (fromRational a >= (Map.!) maxMultMap p)
	  && gcd (numerator a) ((Map.!) primorialMap p) == 1
	  && ((Map.!) supPrimorialMap p) `mod` (numerator a) == 0
	f (a,b,c) (d,e,_) = (a*d, b*e, c)



--module TestPerfect where

import Prelude hiding ((**))
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

import Perfect.Types
import Perfect.Wall
import qualified Math.NumberTheory.Primes.Sieve as Primes

propBricks n = n<0 || map mapper br1 == map mapper br2 where
  p = fromInteger $ Primes.primes !! (n`mod`100000)
  br1 :: [(Rational, Integer)]
  br1 = ratios p
  br2 :: [(Prefactored, Integer)]
  br2 = ratios p
  mapper (r, pa) = (numDen r, pa)

doubleArgs2 mult f a1 a2 b1 b2 = f (a1*mult+a2) (b1*mult+b2)

doubleArgs4 mult f a1 a2 b1 b2 c1 c2 d1 d2 = f (a1*mult+a2) (b1*mult+b2) (c1*mult+c2) (d1*mult+d2)

property1 a b = a<=0 || b<=0 || numDen r1 == numDen r2 where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property2 a b c d = a<=0 || b<=0 || c<=0 || d<=0 || numDen r1 == numDen r2 where
  r1 :: Rational
  r1 = (a%%b)**(c%%d)
  r2 :: Prefactored
  r2 = (a%%b)**(c%%d)

property3 a b = a<=0 || b<=0 || numerFactors r1 == numerFactors r2 where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property4 a b = a<=0 || b<=0 || eq1 r1 == eq1 r2 where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property5 a b = a<=0 || b<=0 || numerEq1 r1 == numerEq1 r2 where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property6 a b c d = a<=0 || b<=0 || c<=0 || d<=0 || numerCoprime r1 m == numerCoprime r2 m where
  m = c * 100 + d
  r1 :: Rational
  r1 = (a%%b)**(c%%d)
  r2 :: Prefactored
  r2 = (a%%b)**(c%%d)

property7 a b c d = a<=0 || b<=0 || c<=0 || d<=0 || numDen r1 == numDen r2 where
  r1 :: Rational
  r1 = (a%%b)**(c%%d)\\(c%%d)
  r2 :: Prefactored
  r2 = (a%%b)**(c%%d)\\(c%%d)

types2 = [property1, property3, property4, property5]
types4' = [property2, property6, property7]
types4 = types4' ++ (map (doubleArgs2 (2^32)) types2) -- ++ (map(doubleArgs2 (2^64)) types2)
types8 = (map (doubleArgs2 (2^32)) types4') -- ++ (map(doubleArgs2 (2^64)) types4)

main = do
  --mapM_ quickCheck types2
  --mapM_ quickCheck types4
  --mapM_ quickCheck types8
  --mapM_ (smallCheck 100) types2
  --mapM_ (smallCheck 10) types4
  --mapM_ (smallCheck 3) types8
  smallCheck 100 propBricks
  quickCheck propBricks

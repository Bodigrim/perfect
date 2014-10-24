module TestPerfect where

import Prelude hiding ((**))
import Test.SmallCheck
import Test.SmallCheck.Series

import Perfect.Types

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
types4 = [property2, property6, property7]

main = do
	mapM_ (smallCheck 100) types2
	mapM_ (smallCheck 10) types4

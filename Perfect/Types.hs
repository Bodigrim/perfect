{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Perfect.Types where

import Prelude hiding ((**))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Ratio
import Data.List
import Math.NumberTheory.Primes.Factorisation

import Test.SmallCheck
import Test.SmallCheck.Series

ourFactorise = factorise'
--ourFactorise = trialDivisionTo maxPrime
--ourFactorSieve = factorSieve maxPrime
--ourFactorise = sieveFactor ourFactorSieve

class FactorizedRatio a where
  (%%) :: Integer -> Integer -> a
  (**) :: a -> a -> a
  (\\) :: a -> a -> a
  numerFactors :: a -> [(Int, Int)]
  eq1 :: a -> Bool
  numerEq1 :: a -> Bool
  numerCoprime :: a -> Integer -> Bool
  numDen :: a -> (Integer, Integer)

instance FactorizedRatio Rational where
  (%%) = ( % )
  (**) = ( * )
  (\\) = ( / )
  numerFactors = map (\(x, y)->(fromInteger x, y)) . (filter ((/=1) . fst)) . ourFactorise . numerator
  eq1 r = numerator r == 1 && denominator r == 1
  numerEq1 r = numerator r == 1
  numerCoprime r m = gcd (numerator r) m == 1
  numDen r = (numerator r, denominator r)

data Prefactored = Prefactored (Map.IntMap Int) (Map.IntMap Int)
  deriving (Show)

instance FactorizedRatio Prefactored where
  n %% d = Prefactored nf df where
    r = n % d
    f = Map.fromList . (map (\(x,y)->(fromInteger x, y))) . (filter ((/=1) . fst)) . ourFactorise
    nf = f $ numerator r
    df = f $ denominator r

  (Prefactored n1 d1) ** (Prefactored n2 d2) = Prefactored n d where
    n' = Map.unionWith (+) n1 n2
    d' = Map.unionWith (+) d1 d2
    cancel = Map.intersectionWith min n' d'
    n = Map.differenceWith f n' cancel
    d = Map.differenceWith f d' cancel
    f a b | a==b = Nothing
          | otherwise = Just (a-b)

  (Prefactored n1 d1) \\ (Prefactored n2 d2) = (Prefactored n1 d1) ** (Prefactored d2 n2)

  numerFactors (Prefactored n _) = Map.toList n

  eq1 (Prefactored n d) = Map.null n && Map.null d

  numerEq1 (Prefactored n _) = Map.null n

  numerCoprime (Prefactored n _) m = all (\d -> m`mod`(toInteger d) /= 0) (Map.keys n)

  numDen (Prefactored n d) = (f n, f d) where
    f xs = Map.foldl (*) 1 $ Map.mapWithKey (\k -> \a -> (toInteger k)^a) xs

type FactRat = Prefactored

property1 a b = a<=0 || b<=0 || (numDen r1 == numDen r2) where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property2 a b c d = a<=0 || b<=0 || c<=0 || d<=0 || numDen r1 == numDen r2 where
  r1 :: Rational
  r1 = (a%%b)**(c%%d)
  r2 :: Prefactored
  r2 = (a%%b)**(c%%d)

property3 a b = a<=0 || b<=0 || (numerFactors r1 == numerFactors r2) where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property4 a b = a<=0 || b<=0 || (eq1 r1 == eq1 r2) where
  r1 :: Rational
  r1 = a%%b
  r2 :: Prefactored
  r2 = a%%b

property5 a b = a<=0 || b<=0 || (numerEq1 r1 == numerEq1 r2) where
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


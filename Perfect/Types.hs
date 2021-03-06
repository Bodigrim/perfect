{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Perfect.Types where

import Prelude hiding ((**))
import Control.Arrow
import qualified Data.IntMap.Strict as Map
import Data.Monoid
import Math.NumberTheory.Primes.Factorisation

ourFactorise :: Integer -> [(Int, Int)]
ourFactorise = map (first fromInteger) . factorise'

(%%) :: Integer -> Integer -> FactRat
n %% d = nf <> fmap negate df
  where
    factMap 1 = mempty
    factMap x = Map.fromAscList . ourFactorise $ x

    g = gcd n d
    nf = factMap (n `div` g)
    df = factMap (d `div` g)

(**) :: FactRat -> FactRat -> FactRat
(**) = Map.mergeWithKey (\_ a b -> let ab = a + b in if ab == 0 then Nothing else Just ab) id id

(\\) :: FactRat -> FactRat -> FactRat
(\\) = Map.mergeWithKey (\_ a b -> let ab = a - b in if ab == 0 then Nothing else Just ab) id (fmap negate)

eq1 :: FactRat -> Bool
eq1 = Map.null

numerEq1 :: FactRat -> Bool
numerEq1 = all (< 0)

-- assuming b has unit denominator
numerCoprime :: FactRat -> FactRat -> Bool
numerCoprime m1 m2 = null $ Map.mergeWithKey
  (\_ a _ -> if a > 0 then Just () else Nothing)
  (const mempty)
  (const mempty)
  m1
  m2

numDen :: FactRat -> (Integer, Integer)
numDen = (f *** f) . Map.partition (> 0) where
  f = Map.foldlWithKey' (\acc k a -> acc * toInteger k ^ a) 1

type FactRat = Map.IntMap Int



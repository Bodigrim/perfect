{-# LANGUAGE LambdaCase #-}

module Perfect.Sigmas where

import Data.Bits
import Math.NumberTheory.Primes

newtype SigmaF = SigmaF { runSigmaF :: Integer -> Integer -> Integer }

sigmaUsualPrimorial :: SigmaF
sigmaUsualPrimorial = SigmaF $
  \p a -> (p ^ (a + 1) - 1) `div` (p - 1) -- sum [ p^b | b<-[0..a]]

sigmaAlterPrimorial :: SigmaF
sigmaAlterPrimorial = SigmaF $
  \p a -> (p ^ (a + 1) + if odd a then (-1) else 1) `div` (p + 1) -- sum [ p^b * (-1)^(a-b) | b<-[0..a]]

sigmaModExpPrimorial :: SigmaF
sigmaModExpPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]

sigmaExpPrimorial :: SigmaF
sigmaExpPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[1..a], a`mod`b == 0 ]

sigmaExpUnitPrimorial :: SigmaF
sigmaExpUnitPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[1..a], a`mod`b == 0 && gcd b (a`div`b) == 1]

sigmaUnitPrimorial :: SigmaF
sigmaUnitPrimorial = SigmaF $
  \p a -> if a==0 then 1 else p^a + 1

sigmaNonUnitPrimorial :: SigmaF
sigmaNonUnitPrimorial = SigmaF $
  \p a -> runSigmaF sigmaUsualPrimorial p a - runSigmaF sigmaUnitPrimorial p a

sigmaInfPrimorial :: SigmaF
sigmaInfPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[0..a], a .|. complement b == -1]

sigmaExpInfPrimorial :: SigmaF
sigmaExpInfPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[1..a], isInfDivisor a b]

sigmaModExpInfPrimorial :: SigmaF
sigmaModExpInfPrimorial = SigmaF $
  \p a -> sum [ p^b | b<-[0..a], isInfDivisor (a+1) (b+1)]

isInfDivisor :: Integer -> Integer -> Bool
isInfDivisor n m = n `mod` m == 0 && and [predicate (maxPrimorial p n) (maxPrimorial p m) | p<-ps] where
  ps = takeWhile (<= m) primes
  maxPrimorial :: Integer -> Integer -> Integer
  maxPrimorial _ 0 = 0
  maxPrimorial p x = if x`mod`p==0 then 1 + maxPrimorial p (x`div`p) else 0
  predicate a b = a .|. complement b == -1

sigmaOrdUnitPrimorial :: SigmaF
sigmaOrdUnitPrimorial = SigmaF $ \case
  2 -> runSigmaF sigmaUsualPrimorial 2
  p -> runSigmaF sigmaUnitPrimorial p

sigmaUnitOrdPrimorial :: SigmaF
sigmaUnitOrdPrimorial = SigmaF $ \case
  2 -> runSigmaF sigmaUnitPrimorial 2
  p -> runSigmaF sigmaUsualPrimorial p

sigmaUSUPPrimorial :: SigmaF
sigmaUSUPPrimorial = SigmaF $ \case
  2 -> runSigmaF sigmaUnitPrimorial 2
  p -> runSigmaF phiUnitPrimorial p

phiUnitPrimorial :: SigmaF
phiUnitPrimorial = SigmaF $ \p a -> p^a - 1

module Perfect.Sigmas where

import Data.Bits
import Math.NumberTheory.Primes.Sieve

type SigmaF = Integer -> Integer -> Integer

sigmaUsualPrimorial p a = sum [ p^b | b<-[0..a]]

sigmaAlterPrimorial p a = sum [ p^b * (-1)^(a-b) | b<-[0..a]]

sigmaModExpPrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmaExpPrimorial p a = sum [ p^b | b<-[1..a], a`mod`b == 0 ]
sigmaExpUnitPrimorial p a = sum [ p^b | b<-[1..a], a`mod`b == 0 && gcd b (a`div`b) == 1]

sigmaUnitPrimorial p a = if a==0 then 1 else p^a + 1
sigmaNonUnitPrimorial p a = sigmaUsualPrimorial p a - sigmaUnitPrimorial p a

sigmaInfPrimorial :: SigmaF
sigmaInfPrimorial p a = sum [ p^b | b<-[0..a], pred b] where
	pred b = a .|. complement b == -1

sigmaExpInfPrimorial :: SigmaF
sigmaExpInfPrimorial p a = sum [ p^b | b<-[1..a], isInfDivisor a b]

sigmaModExpInfPrimorial :: SigmaF
sigmaModExpInfPrimorial p a = sum [ p^b | b<-[0..a], isInfDivisor (a+1) (b+1)]

isInfDivisor n m = n `mod` m == 0 && and [pred (maxPrimorial p n) (maxPrimorial p m) | p<-ps] where
	ps = takeWhile (<= m) primes
	maxPrimorial :: SigmaF
	maxPrimorial p 0 = 0
	maxPrimorial p x = if x`mod`p==0 then 1 + maxPrimorial p (x`div`p) else 0
	pred a b = a .|. complement b == -1


sigmaOrdUnitPrimorial 2 = sigmaUsualPrimorial 2
sigmaOrdUnitPrimorial p = sigmaUnitPrimorial p

sigmaUnitOrdPrimorial 2 = sigmaUnitPrimorial 2
sigmaUnitOrdPrimorial p = sigmaUsualPrimorial p

sigmaUSUPPrimorial 2 = sigmaUnitPrimorial 2
sigmaUSUPPrimorial p = phiUnitPrimorial p

phiUnitPrimorial p a = p^a - 1

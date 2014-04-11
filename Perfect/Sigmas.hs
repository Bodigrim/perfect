module Perfect.Sigmas where

import Data.Bits

sigmaUsualPrimorial p a = sum [ p^b | b<-[0..a]]

sigmaAlterPrimorial p a = sum [ p^b * (-1)^(a-b) | b<-[0..a]]

sigmaModExpPrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmaExpPrimorial p a = sum [ p^b | b<-[0..a], a`mod`b == 0 ]

sigmaUnitPrimorial p a = if a==0 then 1 else p^a + 1
sigmaNonUnitPrimorial p a = sigmaUsualPrimorial p a - sigmaUnitPrimorial p a

sigmaInfPrimorial :: Integer -> Integer -> Integer
sigmaInfPrimorial p a = sum [ p^b | b<-[0..a], pred b] where
	pred b = a .|. complement b == -1

sigmaOrdUnitPrimorial 2 = sigmaUsualPrimorial 2
sigmaOrdUnitPrimorial p = sigmaUnitPrimorial p

sigmaUnitOrdPrimorial 2 = sigmaUnitPrimorial 2
sigmaUnitOrdPrimorial p = sigmaUsualPrimorial p

sigmaUSUPPrimorial 2 = sigmaUnitPrimorial 2
sigmaUSUPPrimorial p = phiUnitPrimorial p

phiUnitPrimorial p a = p^a - 1

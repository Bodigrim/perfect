module Perfect.Sigmas where

sigmaUsualPrimorial p a = sum [ p^b | b<-[0..a]]
sigmaAlterPrimorial p a = sum [ p^b * (-1)^(a-b) | b<-[0..a]]
sigmaModExpPrimorial p a = sum [ p^b | b<-[0..a], (a+1)`mod`(b+1) == 0 ]
sigmaUnitPrimorial p a = if a==0 then 1 else p^a + 1
sigmaNonUnitPrimorial p a = sigmaUsualPrimorial p a - sigmaUnitPrimorial p a

sigmaOrdUnitPrimorial 2 = sigmaUsualPrimorial 2
sigmaOrdUnitPrimorial p = sigmaUnitPrimorial p

sigmaUnitOrdPrimorial 2 = sigmaUnitPrimorial 2
sigmaUnitOrdPrimorial p = sigmaUsualPrimorial p

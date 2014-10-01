module Perfect.Config (maxPrime, maxPower, sigmaPrimorial, perfectness, tryNumber, fileName) where

import Data.Ratio
import Perfect.Sigmas

maxPrime = 10000 :: Integer
maxPower = 100  :: Integer
config = infinitary 3

modExp    = (sigmaModExpPrimorial,    2%1, 6, "output/mod-exp.txt"    )
modExpInf = (sigmaModExpInfPrimorial, 2%1, 6, "output/mod-exp-inf.txt")
expInf    = (sigmaExpInfPrimorial,    2%1, 6, "output/exp-inf.txt"    )
expUnit   = (sigmaExpUnitPrimorial,   2%1, 6, "output/exp-unit.txt"   )

hemiperfect n = (sigmaUsualPrimorial, n%2, 2, "output/" ++ show n ++ "-halfs-perfect.txt")

imperfect n = (sigmaAlterPrimorial, 1%n, m n, "output/" ++ show n ++ "-imperfect.txt") where
	m 5 = 30
	m 3 = 6
	m 6 = 6
	m _ = 2

ouSigma   = (sigmaOrdUnitPrimorial, 2%1, 2, "output/ou-perfect.txt"  )
uoSigma   = (sigmaUnitOrdPrimorial, 2%1, 6, "output/uo-perfect.txt"  )
usupSigma = (sigmaUSUPPrimorial,    1%1, 2, "output/usup-perfect.txt")

infinitary n = (sigmaInfPrimorial, n%1, 6, "output/inf-" ++ show n ++ "-perfect.txt")

tryNumber :: Int
(sigmaPrimorial, perfectness, tryNumber, fileName) = config

module Perfect.Config (maxPrime, maxPower, sigmaPrimorial, perfectness, tryNumber, fileName) where

import Data.Ratio
import Perfect.Sigmas
import Perfect.Types (FactRat, (%%))

maxPrime = 66000
maxPower = 100
config = infinitary 3

type PerfectConfig = (Integer -> Integer -> Integer, FactRat, Int, String)

modExp    = (sigmaModExpPrimorial,    2%%1, 6, "output/mod-exp.txt"    ) :: PerfectConfig
modExpInf = (sigmaModExpInfPrimorial, 2%%1, 6, "output/mod-exp-inf.txt") :: PerfectConfig
expInf    = (sigmaExpInfPrimorial,    2%%1, 6, "output/exp-inf.txt"    ) :: PerfectConfig
expUnit   = (sigmaExpUnitPrimorial,   2%%1, 6, "output/exp-unit.txt"   ) :: PerfectConfig

hemiperfect n = (sigmaUsualPrimorial, n%%2, 2, "output/" ++ show n ++ "-halfs-perfect.txt") :: PerfectConfig

imperfect n = (sigmaAlterPrimorial, 1%%n, m n, "output/" ++ show n ++ "-imperfect.txt") :: PerfectConfig where
	m 5 = 30
	m 3 = 6
	m 6 = 6
	m _ = 2

ouSigma   = (sigmaOrdUnitPrimorial, 2%%1, 2, "output/ou-perfect.txt"  ) :: PerfectConfig
uoSigma   = (sigmaUnitOrdPrimorial, 2%%1, 6, "output/uo-perfect.txt"  ) :: PerfectConfig
usupSigma = (sigmaUSUPPrimorial,    1%%1, 2, "output/usup-perfect.txt") :: PerfectConfig

infinitary n = (sigmaInfPrimorial, n%%1, 6, "output/inf-" ++ show n ++ "-perfect.txt") :: PerfectConfig

tryNumber :: Int
(sigmaPrimorial, perfectness, tryNumber, fileName) = config

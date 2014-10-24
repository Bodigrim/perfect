module Perfect.Config (maxPrime, maxPower, sigmaPrimorial, perfectness, tryNumber, fileName) where

import Perfect.Sigmas
import Perfect.Types (FactRat, (%%))

type PerfectConfig = (SigmaF, FactRat, Int, String)

maxPrime :: Integer
maxPower :: Integer
config :: PerfectConfig

maxPrime = 10000
maxPower = 100
config = imperfect 3

modExp :: PerfectConfig
modExp    = (sigmaModExpPrimorial,    2%%1, 6, "output/mod-exp.txt"    )
modExpInf :: PerfectConfig
modExpInf = (sigmaModExpInfPrimorial, 2%%1, 6, "output/mod-exp-inf.txt")
expInf :: PerfectConfig
expInf    = (sigmaExpInfPrimorial,    2%%1, 6, "output/exp-inf.txt"    )
expUnit :: PerfectConfig
expUnit   = (sigmaExpUnitPrimorial,   2%%1, 6, "output/exp-unit.txt"   )

hemiperfect :: Integer -> PerfectConfig
hemiperfect n = (sigmaUsualPrimorial, n%%2, 2, "output/" ++ show n ++ "-halfs-perfect.txt")

imperfect :: Integer -> PerfectConfig
imperfect n = (sigmaAlterPrimorial, 1%%n, m n, "output/" ++ show n ++ "-imperfect.txt") where
	m 5 = 30
	m 3 = 6
	m 6 = 6
	m _ = 2

ouSigma :: PerfectConfig
ouSigma   = (sigmaOrdUnitPrimorial, 2%%1, 2, "output/ou-perfect.txt"  )
uoSigma :: PerfectConfig
uoSigma   = (sigmaUnitOrdPrimorial, 2%%1, 6, "output/uo-perfect.txt"  )
usupSigma :: PerfectConfig
usupSigma = (sigmaUSUPPrimorial,    1%%1, 2, "output/usup-perfect.txt")

infinitary :: Integer -> PerfectConfig
infinitary n = (sigmaInfPrimorial, n%%1, 6, "output/inf-" ++ show n ++ "-perfect.txt")

sigmaPrimorial :: SigmaF
perfectness    :: FactRat
tryNumber      :: Int
fileName       :: String
(sigmaPrimorial, perfectness, tryNumber, fileName) = config

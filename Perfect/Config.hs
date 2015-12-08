{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Perfect.Config (maxPrime, maxPower, sigmaPrimorial, perfectness, tryNumber, fileName, logName) where

import Numeric.Natural

import Perfect.Sigmas
import Perfect.Types (FactRat, (%%))

type PerfectConfig = (SigmaF, FactRat, Word, String)

fileName :: String
fileName = "output/" ++ filePrefix ++ "-" ++ show maxPrime ++ "-" ++ show maxPower ++ ".txt"

logName :: String
logName = "output/" ++ filePrefix ++ "-" ++ show maxPrime ++ "-" ++ show maxPower ++ ".log"

maxPrime :: Natural
maxPower :: Natural
config :: PerfectConfig

maxPrime = 2000
maxPower = 50
config = modExp

modExp :: PerfectConfig
modExp    = (sigmaModExpPrimorial,    2%%1, 6, "mod-exp"    )
modExpInf :: PerfectConfig
modExpInf = (sigmaModExpInfPrimorial, 2%%1, 6, "mod-exp-inf")
expInf :: PerfectConfig
expInf    = (sigmaExpInfPrimorial,    2%%1, 6, "exp-inf"    )
expUnit :: PerfectConfig
expUnit   = (sigmaExpUnitPrimorial,   2%%1, 6, "exp-unit"   )

hemiperfect :: Natural -> PerfectConfig
hemiperfect n = (sigmaUsualPrimorial, n%%2, 2, show n ++ "-halfs-perfect")

imperfect :: Natural -> PerfectConfig
imperfect n = (sigmaAlterPrimorial, 1%%n, m n, show n ++ "-imperfect") where
  m 5 = 30
  m 3 = 6
  m 6 = 6
  m _ = 2

ouSigma :: PerfectConfig
ouSigma   = (sigmaOrdUnitPrimorial, 2%%1, 2, "ou-perfect"  )
uoSigma :: PerfectConfig
uoSigma   = (sigmaUnitOrdPrimorial, 2%%1, 6, "uo-perfect"  )
usupSigma :: PerfectConfig
usupSigma = (sigmaUSUPPrimorial,    1%%1, 2, "usup-perfect")

infinitary :: Natural -> PerfectConfig
infinitary n = (sigmaInfPrimorial, n%%1, 6, "inf-" ++ show n ++ "-perfect")

sigmaPrimorial :: SigmaF
perfectness    :: FactRat
tryNumber      :: Word
filePrefix     :: String
(sigmaPrimorial, perfectness, tryNumber, filePrefix) = config

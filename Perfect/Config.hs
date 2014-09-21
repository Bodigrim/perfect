module Perfect.Config where

import Data.Ratio
import Perfect.Sigmas

maxPrime = 10000 :: Integer
maxPower = 100  :: Integer
--maxPrimorial = 2^32 :: Integer

---- Mod-exp-inf-perfect
--sigmaPrimorial = sigmaModExpInfPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6
--fileName = "output/mod-exp-inf.txt"

---- Exp-inf-perfect
--sigmaPrimorial = sigmaExpInfPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6
--fileName = "output/exp-inf.txt"

---- Exp-unit-perfect
--sigmaPrimorial = sigmaExpUnitPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6
--fileName = "output/exp-unit.txt"

-- me-perfect
sigmaPrimorial = sigmaModExpPrimorial
perfectness = 2%1 :: Rational
tryNumber = 6
fileName = "output/me-perfect.txt"

---- sigma(n)/n = 5/2
--sigmaPrimorial = sigmaUsualPrimorial
--perfectness = 5%2
--tryNumber = 2
--fileName = "output/5-halfs-perfect.txt"

---- sigma(n)/n = 3
--sigmaPrimorial = sigmaUsualPrimorial
--perfectness = 17%2
--tryNumber = 2
--fileName = "output/17-halfs-perfect.txt"

---- sigma(n)/n = 7/2
--sigmaPrimorial = sigmaUsualPrimorial
--perfectness = 7%2
--tryNumber = 6
--fileName = "output/7-halfs-perfect.txt"

---- 2-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%2 :: Rational
--tryNumber = 2
--fileName = "output/2-imperfect.txt"

-- 3-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%3 :: Rational
--tryNumber = 6
--fileName = "output/3-imperfect.txt"

---- 4-imperfect "Now 576460752303423488"
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%4 :: Rational
--tryNumber = 2
--fileName = "output/4-imperfect.txt"

---- 5-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%5 :: Rational
--tryNumber = 30
--fileName = "output/5-imperfect.txt"

---- 6-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%6 :: Rational
--tryNumber = 6
--fileName = "output/6-imperfect.txt"

---- OU-sigma
--sigmaPrimorial = sigmaOrdUnitPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 2
--fileName = "output/ou-perfect.txt"

---- UO-sigma
--sigmaPrimorial = sigmaUnitOrdPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6
--fileName = "output/uo-perfect.txt"

---- USUP-sigma
--sigmaPrimorial = sigmaUSUPPrimorial
--perfectness = 1%1 :: Rational
--tryNumber = 2
--fileName = "output/usup-perfect.txt"

---- Inf-sigma
--sigmaPrimorial = sigmaInfPrimorial
--perfectness = 2%1
--tryNumber = 6
--fileName = "output/inf-perfect.txt"

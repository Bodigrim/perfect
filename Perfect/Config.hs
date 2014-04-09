module Perfect.Config where

import Data.Ratio
import Perfect.Sigmas

maxPrime = 1000 :: Integer
maxPower = 100  :: Integer

---- me-perfect
--sigmaPrimorial = sigmaModExpPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6

---- sigma(n)/n = 5/2
--sigmaPrimorial = sigmaUsualPrimorial
--perfectness = 5%2
--tryNumber = 2

---- sigma(n)/n = 7/2
--sigmaPrimorial = sigmaUsualPrimorial
--perfectness = 7%2
--tryNumber = 6

-- 2-imperfect
sigmaPrimorial = sigmaAlterPrimorial
perfectness = 1%5 :: Rational
tryNumber = 2

---- 3-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%3 :: Rational
--tryNumber = 6

-- 4-imperfect
--sigmaPrimorial = sigmaAlterPrimorial
--perfectness = 1%4 :: Rational
--tryNumber = 2

---- OU-sigma
--sigmaPrimorial = sigmaOrdUnitPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 2

---- UO-sigma
--sigmaPrimorial = sigmaUnitOrdPrimorial
--perfectness = 2%1 :: Rational
--tryNumber = 6

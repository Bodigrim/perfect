import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName)
import Data.Ratio
import Perfect.Wall (primes, bricks, wall)
import Perfect.Types (FactRat, (%%))

printer :: Either (FactRat, Integer) Integer -> IO()
printer (Left (_, a)) = appendFile fileName (show a ++ "\n")
printer (Right a) = print ("Now " ++ show a)

main =
  mapM_ printer (tryN tryNumber)


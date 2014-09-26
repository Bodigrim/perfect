import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName)
import Data.Ratio
import Perfect.Wall (primes, bricks, wall)

main =
  --mapM (\(_,a) ->  try
  mapM_ (\(r,a) -> if r/= -1%1 then appendFile fileName (show a ++ "\n") else print ("Now " ++ show a)) (tryN tryNumber)
  --mapM (\(r,a) -> if r/= -1%1 then print a else print ("Now " ++ show a)) (tryN tryNumber)


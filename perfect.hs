import Perfect.Tries
import Perfect.Config
import Data.Ratio

main =
  --mapM (\(_,a) -> appendFile "e-perfect.txt" (show a ++ "\n")) try
  mapM (\(r,a) -> if r/= -1%1 then print a else print ("Now " ++ show a)) (tryN tryNumber)


import Perfect.Tries
import Perfect.Config
import Data.Ratio

main =
  --mapM (\(_,a) ->  try
  mapM (\(r,a) -> if r/= -1%1 then appendFile fileName (show a ++ "\n") else print ("Now " ++ show a)) (tryN tryNumber)
  --mapM (\(r,a) -> if r/= -1%1 then print a else print ("Now " ++ show a)) (tryN tryNumber)


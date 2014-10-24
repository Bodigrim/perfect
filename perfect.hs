import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName)
import Perfect.Types (FactRat)

printer :: Either (FactRat, Integer) Integer -> IO()
printer (Left (_, a)) = appendFile fileName (show a ++ "\n")
printer (Right a) = print ("Now " ++ show a)

main :: IO ()
main =
  mapM_ printer (tryN tryNumber)


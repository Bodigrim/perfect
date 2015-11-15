import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName)

printer :: Either Integer Integer -> IO()
printer (Left  a) = appendFile fileName (show a ++ "\n")
printer (Right a) = putStrLn ("Now " ++ show a)

main :: IO ()
main =
  mapM_ printer (tryN tryNumber)


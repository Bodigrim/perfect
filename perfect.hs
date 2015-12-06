import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName, logName)

import System.Directory

printer :: Either Integer Integer -> IO()
printer (Left  a) = do
  putStrLn ("Found " ++ show a)
  appendFile fileName (show a ++ "\n")
printer (Right a) = do
  appendFile logName (show a ++ "\n")
  putStrLn ("Processed " ++ show a)

main :: IO ()
main = do
  logExist <- doesFileExist logName
  alreadyProcessed <- if logExist then (map read . lines) <$> readFile logName else return []
  mapM_ printer (tryN tryNumber alreadyProcessed)


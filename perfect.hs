import Perfect.Tries (tryN)
import Perfect.Config (tryNumber, fileName)

import System.Directory

logName :: String
logName = fileName ++ ".last"

printer :: Either Integer Integer -> IO()
printer (Left  a) = do
  putStrLn ("Found " ++ show a)
  appendFile fileName (show a ++ "\n")
printer (Right a) = do
  writeFile logName (show a)
  putStrLn ("Running at " ++ show a)

main :: IO ()
main = do
  logExist <- doesFileExist logName
  lastPos <- if logExist then (Just . read) <$> readFile logName else return Nothing
  mapM_ printer (tryN tryNumber lastPos)


import Data.List.Split
import Data.List
import Math.NumberTheory.Primes.Factorisation

header = "# List (possibly incomplete) of 4-imperfect numbers.\n\n"

factor :: Integer -> String
factor n = intercalate " " $ map (\(p,a) -> show p ++ (if a==1 then "" else "^" ++ show a) ++ (if (p `elem` obligatory10) && a<10 then " " else "" ) ++ (if (p `elem` obligatory) && a<2 then "  " else "" )) $ factorise n where
	obligatory10 = [2,3]
	obligatory = [2,3,5,7,11,13]

format :: String -> String
format str = replicate (60 - length n) ' ' ++ show number ++ " = " ++ fact where
	(n:a) = splitOn " " str
	number :: Integer
	number = read n
	author = if a==[] then "" else head a
	fact = factor number

main = do
  input <- readFile "4-imperfect.txt"
  putStrLn header
  putStrLn $ intercalate "\n" $ map format $ init $ splitOn "\n" input

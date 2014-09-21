import Data.List.Split
import Data.List
import Math.NumberTheory.Primes.Factorisation

header = "\
\# 3-imperfect numbers > 2^31\n\n\
\# Discovered by:\n\
\# ZZ  - Weiyi Zhou and Long Zhu\n\
\# DJ  - Donovan Johson\n\
\# TDN - T. D. Noe\n\
\# (no mention) - presumably new\n\n\
\# Calculated by Andrew Lelechenko in September 2014\n\n"

factor :: Integer -> String
factor n = intercalate " " $ map (\(p,a) -> show p ++ (if a==1 then "" else "^" ++ show a) ++ (if (p==2 || p==3) && a<10 then " " else "" ) ++ (if (p==2 || p==3) && a<2 then "  " else "" )) $ factorise n

format :: String -> String
format str = replicate (28 - length n) ' ' ++ show number ++ " = " ++ fact ++ replicate (41 - length fact) ' ' ++  (if author=="" then "" else " (" ++ author ++ ")") where
	(n:a) = splitOn " " str
	number :: Integer
	number = read n
	author = if a==[] then "" else head a
	fact = factor number

main = do
  input <- readFile "3-imperfect.txt"
  putStrLn header
  putStrLn $ intercalate "\n" $ map format $ init $ splitOn "\n" input

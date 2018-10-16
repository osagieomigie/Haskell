import Data.Char


productFactorial :: Int -> Int 
productFactorial 0 = 1
productFactorial 1 = 1 
productFactorial n = n * productFactorial(n-1) * prodFact
                  where prodFact = product [1..n-1]


smallestFactor :: Int -> Int 
smallestFactor n = foldr1 min small 
                   where small = [x | x <- allDivisors n, x>=2] 

allDivisors :: Int -> [Int]
allDivisors n = [x | x <- [1..n], n `mod` x == 0]
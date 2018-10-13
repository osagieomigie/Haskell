import Data.Char


productFactorial :: Int -> Int 
productFactorial 0 = 1
productFactorial 1 = 1 
productFactorial n = n * productFactorial(n-1) * prodFact
                  where prodFact = product [1..n-1]
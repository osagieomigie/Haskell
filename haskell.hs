import Data.Char
import Data.List 

productFactorial :: Int -> Int 
productFactorial 0 = 1
productFactorial 1 = 1 
productFactorial n = n * productFactorial(n-1) * prodFact
                  where prodFact = product [1..n-1]

 
smallestFactor :: Int -> Int 
smallestFactor n = foldr1 min small 
                   where small = [x | x <- allDivisors n, x>=2] 

logic :: Int -> Int
logic n = 
      if mod n 2 == 0 then div n 2
      else 3*n+1

gameOddEven :: Int -> [Int]
gameOddEven n =
             if n == 1 then 1:[]
             else n:gameOddEven(logic n)

isGoodPassword :: [Char] -> Bool
isGoodPassword n = (length n >= 8) && (passCheck n)

passCheck :: [Char] -> Bool 
passCheck n
            | ((let upperCheck = filter (`elem`['A'..'Z']) n in upperCheck /= "") && (let lowerCheck = filter (`elem`['a'..'z']) n  in lowerCheck /= "") && (let numCheck = filter (`elem`['0'..]) n in numCheck /= "")) = True  
            | otherwise = False 

isPrime :: Int -> Bool 
isPrime n = 
         if allDivisors n == [1,n] then True
         else False

allDivisors :: Int -> [Int]
allDivisors n = [x | x <- [1..n], n `mod` x == 0]

-- generic tyoe 
matches :: Eq a => a -> [a] -> [a]
matches _ [] = [] 
matches n (x:xs) = 
	            if x == n then n:matches n xs
                else matches n xs 

 
solveQuadraticEquation :: (Double, Double, Double) -> (Double,Double)
solveQuadraticEquation (a,b,c) = (part1,part2)
                            where 
                            	part1  = ((-b+root)/(2*a))
                            	part2  = ((-b-root)/(2*a))	
                            	root = sqrt(b ** 2 - 4 * a * c) 

occursIn :: Eq a => a -> [a] -> Bool
occursIn _ [] = False
occursIn x y = (x `elem` y)

 
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn [][] = True
allOccurIn [] a = True
allOccurIn (x:xs) y =
	                 if x `elem` y then allOccurIn xs y
	                 else False
 
 --rework 
sameElement :: Eq a => [a] -> [a] -> Bool 
sameElement x y 
             | (length x == length y) = allOccurIn x y 
             | otherwise = False


numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences n p = totalL
                     where 
                     	totalL = length [ x | x <- matches n p ] 


allUrls :: String -> [String]
allUrls n = [x | x <- words n, isPrefixOf "http://" x] 


sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (x:xs) = 
            if (isPrime x) then x:sieve xs
            else sieve xs 


pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = 1:zipWith (+) mid (tail mid)++[1]
          where
            mid = pascal (n-1)

-- Collapse an Ntree type to an array 
data NTree = Leaf Int | Node NTree Int NTree
collapse :: NTree -> [Int] 
collapse (Leaf n) = n:[]
collapse (Node left n right) =  foldr (:) (n:r) (l)
               where 
                 r = collapse right
                 l = collapse left
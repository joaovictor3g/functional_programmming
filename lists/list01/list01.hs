-- 1
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
minorOfTwo :: Int -> Int -> Int
minorOfTwo = min

-- 2
minorOfThree :: Int -> Int -> Int -> Int
minorOfThree x y z = min z (min x y)

-- 3
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

fat :: Int -> Int
fat n = case n of 0 -> 1
                  1 -> 1
                  value -> n * fat (n - 1)
                  

-- 4
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fib :: Int -> Int
fib n
  | n == 0 = 1
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = fib (n - 1) + fib (n - 2)


-- 5
element :: Int -> [Int] -> Int
element n u = u !! n

-- 6
contains :: Int -> [Int] -> Bool
contains x u = x `elem` u

-- 7
total :: [Int] -> Int
total u = if length u == 1 then 1 else 1 + total (init u)

total' [] = 0
total' (x:xs) = 1 + total' xs

-- 8
bigger :: [Int] -> Int
bigger  = maximum

-- 9
frequency :: Int -> [Int] -> Int
frequency x u = length [y | y <- u, x == y]

-- 10
unique :: Int -> [Int] -> Bool
unique x u = length [y | y <- u, x == y] == 1

-- 11
biggersThan :: Int -> [Int] -> [Int]
biggersThan x u = [y | y <- u, y > x]


-- 12
concat' :: [Int] -> [Int] -> [Int]
concat' l1 l2 = l1 ++ l2

-- 13
tail' :: [Int] -> [Int]
tail'  = tail

-- 14
body :: [Int] -> [Int]
body = init

-- 15
appears :: [Int] -> Int -> Int
appears u n = length [x | x <- u, x == n]

extract :: Eq a => [a] -> a -> [a]
extract u n = [x | x <- u, x /= n]

unique' :: [Int] -> [Int]
unique' [] = []
unique' (x:xs) = x: unique' (extract xs x)


-- 16

minors :: Int -> [Int] -> [Int]
minors n [] = []
minors n (x:xs) = [a | a <- xs, a <= x] ++ minors n xs

-- 17
alter :: Int -> [Int]
alter n = [((-1)^x) * div x 2  | x <- [2..(2 * n + 1)]]

-- 18
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 19
divide :: [Int] -> Int -> ([Int], [Int])
divide u n = splitAt n u


-- 20
intercal :: [Int] -> [Int] -> [Int]
intercal a [] = a
intercal [] b = b
intercal (x:xs) b = x:head b:intercal xs (tail b)

-- 21
union' :: [Int] -> [Int] -> [Int]
union' a b = unique' (a ++ b)

-- 22
-- intersec :: [Int] -> [Int] -> [Int]
-- intersec a [] = a
-- intersec [] b = b
-- intersec a@(x:xs) b@(y:ys) = if appears (unique' (a ++ b)) x > 1
--                       then x:intersec xs ys
--                       else if appears (unique' (a ++ b)) y > 1
--                       then y:intersec xs ys
--                       else intersec xs ys

-- 23

sequency :: Int -> Int -> [Int]
sequency n m = [m..(m + n - 1)]

-- 24

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) = if n <= x
                  then n:x:xs
                  else x:insert n xs

-- 25
isSorted :: [Int] -> Bool
isSorted [] = error "Empty List"
isSorted [a] = True
isSorted (x:y:xs) = (x <= y) && isSorted (y:xs)

-- 26 

qSort :: [Int] -> [Int]
qSort u = u 

-- 27
rotEsq :: Int -> String -> String
rotEsq n all@(x:xs) = if n > 0
                  then rotEsq (n - 1) xs ++ [x]
                  else all  

-- 28

rotDir :: Int -> String -> String
rotDir n s = if n > 0
                  then last s:rotDir (n - 1) (init s)
                  else s  

-- 29
-- toUpper c = 
findCharIndex :: Char -> Int
findCharIndex c = if c `elem` ['a'..'z']
              then length ['a'..c] - 1
              else
                length ['A'..c] - 1

getCorrespondingChar :: Char -> String -> Char
getCorrespondingChar c s = if c /= ' '
                              then s !! findCharIndex c
                              else ' '

upper :: String -> String
upper s = [getCorrespondingChar x ['A'..'Z'] | x <- s]

-- 30 👽
capitalizeFst :: [Char] -> [Char]
capitalizeFst (x:xs) = getCorrespondingChar x ['A'..'Z'] : xs

title :: String  -> String
title s = [if c /= ' ' then getCorrespondingChar c ['a'..'z'] else getCorrespondingChar c ['A'..'Z'] | c <- s]

-- 31

selec :: [Char] -> [Int] -> [Char]
selec _ [] = ""
selec s (x:xs) = s!!x : selec s xs

-- 32

isPalind :: String -> Bool
isPalind s = s == reverse' s

-- 33

prime' :: Int -> Bool
prime' n = null [x | x <- [2..(n - 1)], n `mod` x == 0]

-- 34

sdig :: Integral t => t -> t
sdig n
  | n < 10 = n
  | otherwise = n `mod` 10 + sdig (n `div` 10)

-- 35


-- 36

-- 37

splitints :: Integral a => [a] -> ([a], [a])
splitints u = ([x | x <- u, odd x], [x | x <- u, even x])

-- 38
perfect :: Integral a => a -> Bool
perfect n = not (null ([x | x <- [1..n], mod (n^n) x == 0]))

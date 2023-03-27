minorOfTwo :: Int -> Int -> Int
minorOfTwo = min

minorOfThree :: Int -> Int -> Int -> Int
minorOfThree x y z = min z (min x y) 

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  

element :: Int -> [Int] -> Int
element n u = u !! n

contains :: Int -> [Int] -> Bool
contains x u = x `elem` u

total :: [Int] -> Int
total u = if length u == 1 then 1 else 1 + total (init u) 

total' [] = 0
total' (x:xs) = 1 + total' xs

bigger :: [Int] -> Int
bigger u = maximum u

frequency :: Int -> [Int] -> Int
frequency x u = length [y | y <- u, x == y] 

unique :: Int -> [Int] -> Bool
unique x u = length [y | y <- u, x == y] == 1

biggersThan :: Int -> [Int] -> [Int]
biggersThan x u = [y | y <- u, y > x]

concat' :: [Int] -> [Int] -> [Int]
concat' l1 l2 = l1 ++ l2

tail' :: [Int] -> [Int]
tail' u = tail u

body :: [Int] -> [Int]
body u = init u

unique' :: [Int] -> [Int]
unique' [] = []
unique' (x:xs) = if x `notElem` xs then [x] ++ unique' xs else unique' xs

-- minors :: Int -> [Int] -> [Int]
-- minors n (x:xs) = if length minors n == n   

-- alter :: Int -> [Int]
-- alter n = [(-x) / (-1) | x <- [1..n]]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

divide :: [Int] -> Int -> ([Int], [Int])
divide u n = (take n u, drop n u)

intercal :: [Int] -> [Int] -> [Int]
intercal a b = if length a == 1 then [head a] ++ [head b] else intercal (tail a) (tail b) 

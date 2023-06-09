-- Pattern Matching

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

third :: (a,b,c) -> c
third (_,_,c) = c

head' :: [a] -> a
head' [] = error "Can't call head on a empty list, dummy!"
head' (x:_) = x

clynder :: Double -> Double -> Double
clynder r h = 
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^ 2
 in sideArea + 2 * topArea 






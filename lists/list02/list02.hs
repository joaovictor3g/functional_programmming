-- 1
parity :: [Bool] -> Bool
parity u = odd (length (filter (==True) u))

-- 2
rev :: Int -> Int
rev x
  | x < 10 = x
  | otherwise = (x `mod` 10) * 10 ^ (length (show x) - 1) + rev (x `div` 10)

-- 3
delete' :: (Eq a) => a -> [a] -> [a]
delete' x (y:ys)
 | x == y = ys
 | otherwise = y: delete' x ys

-- 4
swap :: [a] -> Int -> Int -> [a]
swap u p q = take p u ++ drop (p + 1) u ++ [u !! p] 

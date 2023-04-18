order (x:xs) = foldl (\acc y -> if y <= minimum acc then y:acc else acc) [x] xs

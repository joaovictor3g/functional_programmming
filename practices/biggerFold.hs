bigger (x:xs) = foldl (\acc b -> max acc b) x xs

onlyUppercase xs = foldl (\acc y -> if y `elem` ['A'..'Z'] then acc ++ [y] else acc) "" xs

map' f xs = [f n | n <- xs]

foldr' f acc xs = foldr f acc xs

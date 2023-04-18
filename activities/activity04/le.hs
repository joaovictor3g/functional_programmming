menor [a] = a
menor (x:xs)
 | x < menor xs= x
 | otherwise = menor xs

delete'min [a] = []
delete'min all@(x:xs)
 | x == menor (all) = xs
 | otherwise = x: delete'min xs

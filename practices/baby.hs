doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber x = if x > 100 then x else x * 2

setList list i x = if i > 0 && i < length list then take i list ++ [x] ++ drop (i + 1) list else list   

triples = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]

rightTriangles' = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a + b + c == 24]

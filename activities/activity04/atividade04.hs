-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "474110" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Victor Dias Barroso" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Implementar função que receba uma lista
-- ou string de entrada e retorne uma outra 
-- equivalente sem repetiições de elementos,


extract :: Eq a => [a] -> a -> [a]
extract u n = [x | x <- u, x /= n]

unique :: Eq s => [s] -> [s]
unique [] = []
unique [a] = [a]
unique (x:xs) = x: unique (extract xs x)


-- Exemplos:

-- >> unique "a1abaa1123b"
-- "ab23"
-- >> unique [2,1,1,3,3,1,1,3,2
-- [2,1,3]]

-- Obs: (1) Note que a ordem relativa das chaves
-- remanescentes se preserva. (2) Se existir uma função em
-- Haskell que faça a mesma coisa, não deve ser usada. 


-- 2

-- Construa função que remova o valor mínimo de uma lista. 
findMin :: (Ord a) => [a] -> a
findMin [] = error "empty list"
findMin [a] = a
findMin (x:xs) = min x (findMin xs)

findMinIndex :: (Num a1, Ord a2) => [a2] -> a1
findMinIndex [] = error "empty list"
findMinIndex [a] = 0
findMinIndex (x:xs) = if x <= findMin xs 
                      then 0
                      else 1 + findMinIndex xs 

delete'min :: (Ord a) => [a] -> [a]
delete'min [] = []
delete'min x = take (findMinIndex x) x ++ drop (findMinIndex x + 1) x


-- Exemplos,

-- >> delete'min [1,3,2,5]
-- [3,2,5]
-- >> delete'min [7,3,2,5,6]
-- [7,3, 5,6]

-- Obs: (1) Se o valor mínimo se repetir
-- então somente a primeira aparição deve 
-- ser removida. (2) Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não deve se utilizada

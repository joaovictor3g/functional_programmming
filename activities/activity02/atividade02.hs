-- IDENTIFICAÇÃO
matricula = "474110"

-- Nome
nome = "João Victor Dias Barroso"

-- ATIVIDADE 2

-- Esta atuvidade visa construir uma 
-- função que determine os n primeitos números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [y | y <- [2..x], x `mod` y == 0]

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = length (divisores x) == 1 -- Divisível somente por ele mesmo.

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n [x | x <- [1..], eprimo x]




-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "474110" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Victor Dias Barroso" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,
-- Cinstrua função que 
-- receba uma string e 
-- retorne a lista das 
-- tuplas das frquencias dos
-- seus caracteres

removeChar :: [Char] -> Char -> [Char]
removeChar s x = [y | y <- s, y /= x]

countCharEquals :: [Char] -> Char -> Int
countCharEquals s a = foldl (\acc y -> if a == y then 1 + acc else acc) 0 s 

freq :: [Char] -> [(Char, Int)]
freq "" = []
freq (x:xs)
  | null xs  = [(x, 1)]
  | otherwise = (x, countCharEquals (x:xs) x): freq (removeChar xs x) 

{- 
  [('c', 1)] ++ freq "asa"
  [('c', 1), ('a', 2)] ++ freq "s"
  [('c', 1), ('a', 2), ('s', 1)] ++ freq ""
  [('c', 1), ('a', 2), ('s', 1)]
-}
-- Exemplos:

-- >> freq "abcdaadd"
-- [('a',3), ('b',1),('c',1),('d',3)]
-- >> freq "A casa"
-- [('A',1), ('a', 2), ('c',1), ('s', 1), (' ',1) ]

-- Se existir uma função em
-- Haskell que faça a mesma coisa, não use.

-- 2

-- Construa função que ordene
-- a lista de tuplas da questão
-- por valor de frequencia,

quicksort :: [(Char, Int)] -> [(Char, Int)] 
quicksort [] = []
quicksort (x:xs) =
  quicksort [a | a <- xs, snd a <= snd x] ++ [x] ++ quicksort [a | a <- xs, snd a > snd x]


freqSort :: [(Char, Int)] -> [(Char, Int)]
freqSort = quicksort

-- Exemplos,

-- >> s = freqSort freq "aaaa22p"
-- [('p',1), ('2', 2), ('a', 4)]

-- Obs: Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não use.

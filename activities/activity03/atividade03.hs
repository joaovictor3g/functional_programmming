-- IDENTIFICAÇÃO
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
matricula = "474110" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Victor Dias Barroso" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip all@(x:xs)
  | null all = ""
  | null xs = [c | c <- [x], c /= ' ']
  | x == blankStr = strip xs
  | last xs == blankStr = strip (init all)
  | otherwise = all
  where blankStr = ' '


-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord all@(x:xs)
  | null all = ("", "")
  | null xs = ([x], "")
  | x == ' ' = ([x], xs)
  | otherwise = (strip (x: fst (popWord xs)), strip (snd (popWord xs)))



-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

breakStr :: [Char] -> [Char]
breakStr all@(x : xs)
  | x == ' ' = ""
  | xs == " " = ""
  | null [c | c <- all, c == ' '] = all
  | otherwise = x: breakStr xs

splitStr :: [Char] -> [[Char]]
splitStr all@(x:xs)
  | null all = [""]
  | null xs = [[x]]
  | null [c | c <- all, c == ' '] = [x:xs]
  | x == ' ' = splitStr (strip xs)
  | otherwise = breakStr all : splitStr (drop pieceLenght xs)
  where pieceLenght = length (breakStr all)

splitStr' :: [Char] -> [[Char]]
splitStr' xs
  | null xs = [""]
  | null [c | c <- xs, c == ' '] = [xs]
  | xs == " " = [""]
  | otherwise = fst (popWord xs) : splitStr' (snd (popWord xs))

splitStr'' :: [Char] -> [[Char]]
splitStr'' "" = [""]
splitStr'' xs = take 1 xs : splitStr'' (drop 1 xs)
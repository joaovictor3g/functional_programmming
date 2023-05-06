-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "474110" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Victor Dias Barroso" -- coloque seu nome aqui entre aspas

-- 1

-- ATIVIDADES A FAZER
-- Construa funções que

data List a = Empty | Node a (List a) 
 deriving Show

-- Mapeia uma  lista noutra. As tais
-- devem possuir tipo List.  A função de
-- mapeamento é o primeiro argumento.
--A lista a processar é o segundo.
-- Não modifique a assinatura.

listMap :: (t -> a) -> List t -> List a
listMap _ Empty = Empty
listMap fn (Node a ls) = Node (fn a) (listMap fn ls)


-- exemplo
-- >> let x = Node 5 (Node 3 (Node 7 (Node 1 (Node 9 Empty))))
-- >>  listMap (\n->n+1) x
-- Node 6 (Node 4 (Node 8 (Node 2 (Node 10 Empty)))) 

-- ----------------------
-------------------------

-- Transforma lista List numa string 
-- cujas chaves são separadas por ":".
-- Não modifique a assunatura.
listToStr :: Show a => List a -> [Char]
-- listToStr :: List Char -> [Char]
listToStr Empty = ""
listToStr (Node c Empty) = show c
listToStr (Node c ls) = show c ++ ":" ++ listToStr ls  

-- exemplo
-- >> let x = Node 5 (Node 3 (Node 7 (Node 1 (Node 9 Empty))))
-- >> listToStr x
-- "5:3:7:1:9"


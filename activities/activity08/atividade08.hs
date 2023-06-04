-- MÓDULOS

import System.IO
import System.Environment
-- não import mais nada!

--IDENTIFICAÇÃO

atividade = 8
matricula = "474110"
nome      = "João Victor Dias Barroso"

-- ATIVIDADE

-- Construir programa que leia
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa deve susturuir
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações deve ser
-- a saída. Seu nome precisa se o de f
-- o arquivo de saída deter nome igual
-- com prefixo "subst-". .

-- MATENHA O .hs COM NOME
-- "atividade.hs" E CONSEQUENTEMENTE
-- EXECUTÁVEL COMO SENDO
-- "atividade08".

-- CÓDIGO


main = do
    args <- getArgs
    let [f, w1, w2] = args
    content <- readFile f
    writeFile ("subst-" ++ f) (
      unlines (
        foldl (
          \acc wordsL -> acc ++ [unwords (toggleWords wordsL w1 w2)]
        ) [] (lines content)))

    return ()

toggleWords :: String -> [Char] -> [Char] -> [[Char]]
toggleWords f w1 w2 = foldl (
  \acc2 word -> if fst (handleSpecialChar word) == w1 
                then acc2 ++ [w2  ++ snd (handleSpecialChar word)] 
                else acc2 ++ [word]) [] (words f)

handleSpecialChar :: [Char] -> ([Char], [Char])
handleSpecialChar w = (
  [x | x <- w, x `elem` ['A'..'Z'] || x `elem` ['a'..'z']], 
  [x | x <- w, x `notElem` ['A'..'Z'] && x `notElem` ['a'..'z']]
  )

-- INFORMAÇÕES

-- Compilação e execução

-- $ ghci atividade-08.hs
-- $ ./atividade-08 historia.txt Pedro Pablo

-- Onde "historia.txt" é um arquivo de texto
-- em que toda palavra "Pedro" é substituída
-- por "Pablo".

-- Exemplo

-- "historia.txt" de entrada,

-- Pedro vivia numa casa de pedra.
-- Mas Pdro queria morar numa 
-- casa de ouro. Pobre Pedro!

-- "subst-historia.txt" criado,

-- Peblo vivia numa casa de pedra.
-- Mas Pabloqueria morar numa 
-- casa de ouro. Pobre Pablo!
-- 





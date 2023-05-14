
-- IDENTIFICAÇÃO

atividade = 7

nome = "João Victor Dias Barroso"

matricula = "474110"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
    show (Poly xs)
      | null xs = ""
      | last xs == 0 = showInits
      | length xs == 1 = show (head xs)
      | length xs == 2 = show (head xs) ++ lastEl ++ xEvaluate2
      | otherwise = showInits ++ lastEl ++ xEvaluate ++ show exp
      where lastEl = (\n -> if n < 0 then show n else "+" ++ show n) (last xs)
            initEls = init xs
            exp = length xs - 1
            showInits = show (Poly initEls)
            xEvaluate = "x^"
            xEvaluate2 = "x"
            plus = "+"

-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly xs) x
  | null xs = 0.0
  | otherwise = last xs * x ^ (length xs - 1) + avalPoly (Poly (init xs)) x

-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0

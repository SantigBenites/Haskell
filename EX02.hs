

{-

8. Determine o valor de cada expressão:

a) [2*x | x <- [1,2,3]]
-}

--ex8a = [2,4,6]


{-
b) [x^2 | x <- [1..8], x `mod` 2 == 0]
-}

--ex8b = [4,16,36,64]

{-
c) [x | x <- ['6'..'S'], isDigit x]
-}

--ex8c = [6,7,8,9]

{-
d) [(x,y)| x <- [1..3], odd x, y <- [1..3]]
-}

--ex8d = [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)]

{-
e) [(x,y)| x <- [1..3], y <- [1..3], odd x]
-}

--ex8e = [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)]

{-

9. Utilizando uma lista em compreensão escreva uma expressão que calcule a soma 
1^2 + 2^2 + ... + 100^2 dos quadrados dos primeiros 100 inteiros.

-}

somaQuadradosPrimeirosInteiros :: Int
somaQuadradosPrimeirosInteiros = sum [x^2 | x <- [1..100]]


{-
11. Dizemos que um inteiro positivo é perfeito se é igual à soma de todos os 
seus fatores, excluindo o próprio número.

(a) Utilizando uma lista em compreensão escreva a função fatores, que devolve os
fatores do inteiro dado (por uma qualquer ordem).
-}

fatores :: Int -> [Int]
fatores n = [x | x <- [1..n - 1], n `mod` x == 0]

{-

(b) Defina agora uma função perfeitos que calcula a lista de todos os números 
perfeitos até um dado limite.

-}

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1 .. n], sum( fatores(x) ) == x]

{-
12. Defina a lista infinita com todas as potências de dois.
-}

potenciasDeDoisInfinita :: [Int]
potenciasDeDoisInfinita = [2^x | x <- [0 .. ]]

{-
13. Defina a função produtoEscalar que calcula o produto escalar de dois 
vetores. 

Assuma que cada vetor é representado por uma lista e que as duas listas
têm o mesmo comprimento.
-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [fst n * snd n | n <- zip xs ys]

{-
17. Defina a função pares de modo a que pares n seja a lista composta por todos 
os valores (i, j) com 1 ≤ i, j ≤ n que satisfaçam a condição i ∕= j.
-}

pares :: Int -> [(Int, Int)]
pares n = [(x ,y)| x <-[1 .. n] , y <-[1 .. n] , x/=y]

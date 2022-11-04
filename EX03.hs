-- Cap 2, 1
pontoTri :: (Int,Int,Int)
pontoTri = (1,2,3)

numero :: Int
numero = 7

poligno :: [(Float,Float)]
poligno = [(1,2),(3,4),(5,6)]

aluno :: (String,Int,(String,Int))
aluno = ("Santiago" , 54392 ,("Principios de Programacao",1231231))

alunos :: [(String,Int,(String,Int))]
alunos = [("Santiago" , 54392 ,("Principios de Programacao",1231231)),("Macaco" , 22455 ,("analise",24554551)) ]

paragrafo :: [String]
paragrafo = ["What","is","Love","Baby","Dont","hurt","me","more","Dont","hurt","me","no"]

texto :: [[String]]
texto = [["What","is","Love","Baby","Dont","hurt","me","more","Dont","hurt","me","no"],["What","is","Love","Baby","Dont","hurt","me","more","Dont","hurt","me","no"]]


-- Cap 2, 2

--['a','b','c'] - Sim
--('a','b','c') - Sim
--['a',True] - Nao
--[True, False] - Sim
--["a disciplina de PP", "é fixe"] - Sim
--[('a',False),('b',True)] - Nao
--[isDigit 'a', isLower 'f', isUpper 'h'] -- Nao
--(['a','b'],[False,True]) - Nao
--[['a','b'],[False,True]] - Nao
--[isDigit, isLower, isUpper] - Nao



-- Cap 3, 1
primeiroElementoDeUmPar :: (a, b) -> a
primeiroElementoDeUmPar (a,_) = a

trocaOrdemTuplo :: (a, b) -> (b, a)
trocaOrdemTuplo (a,b) = (b, a)

primeiroElementoDeUmTriplo :: (a, b ,c) -> a
primeiroElementoDeUmTriplo (a,_,_) = a

trocaOrdemTriplo :: (a, b ,c) -> (b, a ,c)
trocaOrdemTriplo (a, b ,c) = (b,a,c)

segundoDeLista :: [a] -> a
segundoDeLista (_:y:_) = y 

segundoDoPrimeiroPar :: [(a,b)] -> b
segundoDoPrimeiroPar ((_,b):_) = b

-- Cap 3, 3
-- A primeira e a ultima sao iguais a segundo apenas pode ser utilizada para Ints

-- Cap 3, 4

-- Cap 3, 5

quadrante :: (Float ,Float) -> Int
quadrante (x,y)
    | x >= 0 && y > 0 =1
    | x <  0 && y > 0 =2
    | x <  0 && y < 0 =3
    | otherwise       =4
                 
-- Cap 3, 8
safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

safeTail' :: [a] -> [a]
safeTail' xs
    | null xs = []
    | otherwise  = tail xs

safeTail'' :: [a] -> [a]
safeTail'' [] = []
safeTail'' (_:xs) = xs
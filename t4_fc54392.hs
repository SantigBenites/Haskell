module Eleicoes (Candidato(..),Estado(..),Nacao,criaNacao,obterEstado,adicionaVotosEstado,adicionaVotosNacao,vencedorEstado,vencedorEleicao) where

-- Santiago Benites,54392,2020/2021
-- fc54392@alunos.fc.ul.pt

-- Cria um data type para representar um de dois Candidatos de nome A e B
data Candidato = A | B deriving (Eq, Show)

{- Cria um data type para representar um estado de uma nacao, este nessecita de ter
 um nome, um peso (numero de representantes), o numero de votos para o candidato A
 e o numero de votos para o candidato B
-}
data Estado = Estado { nome :: String  
                     , peso :: Int  
                     , votosA :: Int  
                     , votosB :: Int  
                     }

{- Cria um data type que reprensenta uma Nacao, esta nessecita de uma lista de estados 
que pertencem a essa nacao -}
type Nacao = [Estado]

{-Cria uma nacao usando uma lista de pares (String, Int), sendo a String o nome 
do estado e o Int o peso desse estado -}
criaNacao :: [(String, Int)] -> Nacao
criaNacao xs = foldr (\x acc -> (Estado{nome = (fst x) , peso = (snd x), votosA = 0, votosB = 0}):acc) [] xs

{- Dada uma nacao n e uma String s, esta funcao obtem o estado em n com o nome s
-}
obterEstado :: Nacao -> String -> Estado
obterEstado [] _ = Estado {nome = "", peso = 0, votosA = 0, votosB = 0}
obterEstado (x:xs) s 
    | nome x == s = x
    | otherwise = obterEstado xs s

{-Dado um estado E, um int A e um int B, esta funcao adiciona A a votosA de E e B a votosB de E
-}
adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado (Estado n p a b) voteA voteB = Estado n p (a+voteA) (b+voteB)

{- Dada uma nacao N e uma lista xs do tipo [(String,Int,Int)], sendo [(nomeEstado,votosA,votosB)], para cada estado com nomeEstado em nacao
 os votos dessa nacao serao adiconados aos respetivos votos na lista  e esse valor posto num novo estado com o mesmo nome, 
 e todos esses estados sao postos numa novo nacao, essa que e a nacao que a funcao da return.
-}
adicionaVotosNacao :: Nacao -> [(String,Int,Int)] -> Nacao
adicionaVotosNacao [] _ = []
adicionaVotosNacao n xs= foldr (\x acc -> (adicionaVotosEstado x (fst(obterVotos (nome x) xs)) (snd(obterVotos (nome x) xs)) ):acc ) [] n

{- Dada uma String s e uma lista xs, obtem a soma de todos os votos para um certo estado numa lista xs, ou seja, todas 
 as instancias em xs com String == s
-}
obterVotos :: String -> [(String,Int,Int)] -> (Int,Int)
obterVotos _ [] = (0,0)
obterVotos s ((s1,a,b):xs)
    | s == s1 = (a + fst(obterVotos s xs),b + snd(obterVotos s xs))
    | otherwise = obterVotos s xs

{- Dado um estado E, esta funcao da return de um Maybe Candidato, este Candidato sendo o candidato com mais votos no estado E, 
 caso os votos sejam iguais ira dar return de Nothing
-}
vencedorEstado :: Estado -> (Maybe Candidato)
vencedorEstado (Estado _ _ a b)
    | a/=b = Just maximo
    | otherwise = Nothing
    where maximo = if(a > b) then A else  B

{- Dado uma nacao N, esta funcao da return de um Maybe Candidato, este Candidato sendo com mais representantes na nacao N, 
 caso o nuemro de representantes sejam iguais ira dar return de Nothing.
-}
vencedorEleicao :: Nacao -> (Maybe Candidato)
vencedorEleicao n
    | voteA/=voteB = Just maximo
    | otherwise = Nothing
    where voteA = foldr (\x acc -> if((vencedorEstado x) == Just A)then acc + (peso x) else acc) 0 n
          voteB = foldr (\x acc -> if((vencedorEstado x) == Just B)then acc + (peso x) else acc) 0 n
          maximo = if(voteA > voteB) then A else  B



instance Eq Estado where
    a == b  = (peso a == peso b )&&(vencedorEstado a == vencedorEstado b)


instance Show Estado where
    show (Estado n p a b) = n ++ " " ++ Prelude.show p ++ " " ++ Prelude.show a ++ " " ++ Prelude.show b
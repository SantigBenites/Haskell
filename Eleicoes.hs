module Eleicoes (Candidato,Estado,Nacao,criaNacao,obterEstado,adicionaVotosEstado,adicionaVotosNacao,obterVotos,vencedorEstado,vencedorEleicao) where

import Data.List

data Candidato = A | B deriving (Eq, Show)

data Estado = Estado { nome :: String  
                     , peso :: Int  
                     , votosA :: Int  
                     , votosB :: Int  
                     }

type Nacao = [Estado]

criaNacao :: [(String, Int)] -> Nacao
criaNacao xs = foldr (\x acc -> (Estado{nome = (fst x) , peso = (snd x), votosA = 0, votosB = 0}):acc) [] xs

obterEstado :: Nacao -> String -> Estado
obterEstado [] _ = Estado {nome = "", peso = 0, votosA = 0, votosB = 0}
obterEstado (x:xs) s 
    | nome x == s = x
    | otherwise = obterEstado xs s

adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado (Estado n p a b) voteA voteB = Estado n p (a+voteA) (b+voteB)

adicionaVotosNacao :: Nacao -> [(String,Int,Int)] -> Nacao
adicionaVotosNacao [] _ = []
adicionaVotosNacao n xs= foldr (\x acc -> (adicionaVotosEstado x (fst(obterVotos (nome x) xs)) (snd(obterVotos (nome x) xs)) ):acc ) [] n

obterVotos :: String -> [(String,Int,Int)] -> (Int,Int)
obterVotos _ [] = (0,0)
obterVotos s ((s1,a,b):xs)
    | s == s1 = (a + fst(obterVotos s xs),b + snd(obterVotos s xs))
    | otherwise = obterVotos s xs

vencedorEstado :: Estado -> (Maybe Candidato)
vencedorEstado (Estado _ _ a b)
    | a/=b = Just maximo
    | otherwise = Nothing
    where maximo = if(a > b) then A else  B

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
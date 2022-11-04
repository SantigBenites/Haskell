--Santiago Benites fc54392

listaDeAlguidaresVazia :: Ord a => [[a]]                                                         -- Cria lista de alguidares vazia
listaDeAlguidaresVazia = []

adicionaAListaDeAlguidares :: Ord a => Int -> a -> [[a]] -> [[a]]                                -- adiciona um elemnto a a uma lista de alguidares e
adicionaAListaDeAlguidares x y e = divide x (qsort(concat(adicionaAListaDeAlguidaresAux y 1 e))) -- coloca o elemento no algudar junta todos os alguidares numa lista e divide-os em alguidares de tamanaho x

adicionaAListaDeAlguidaresAux :: Ord a => a -> Int -> [[a]] -> [[a]]                             -- analiza as cabecas dos alguidares para saber em qual colocar o elemento
adicionaAListaDeAlguidaresAux x _ [[]] = [[x]] 
adicionaAListaDeAlguidaresAux x n e
  | n >= length e = e ++ [[x]]                                                                   -- Condicao de fim de recursao (se o numero do alguidar que estamos a analizar for maior que o tamanho de e)
  | head (e!!n) >= x = (take (n-1) e) ++ [qsort(x:(e!!(n-1)))] ++ (drop n e)                     -- coloca no alguidar no elemento x e reconstruir a lista de alguidares
  | otherwise = adicionaAListaDeAlguidaresAux x (n+1) e                                          -- alguidar nao e valido para colocar o elemento x

divide :: Ord a => Int -> [a] -> [[a]]                                                           -- Divide uma lista xs em alguidares de tamanho n
divide _ [] = []
divide n xs
  | n > 0 = (take n xs) : (divide n (drop n xs))
  | otherwise = error "Negative or zero n"

qsort :: (Ord a) => [a] -> [a]                                                                   -- Quicksort
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]

elemListaDeAlguidares :: Ord a => a -> [[a]] -> Bool                                             -- Analiza uma lista de alguidares e ve se um elemnto se encontra nesta lista
elemListaDeAlguidares n e = length(filter (==n) (concat e))>0                                    -- Junta todos os elemntos numa lista e da filter por elementos == a n se o tamanho da lista for >0 existe esse elemento

removerDaListaDeAlguidares :: Ord a => a -> [[a]] -> [[a]]                                       -- Remove de uma lista de alguidares um elemento
removerDaListaDeAlguidares x e = delete(removerDaListaDeAlguidaresAux x e 0)


removerDaListaDeAlguidaresAux :: Ord a => a -> [[a]] -> Int -> [[a]]                             -- Tal como o adicionaAListaDeAlguidaresAux procura todos os alguidares por x se encontrar 
removerDaListaDeAlguidaresAux _ [[]] _ = []                                                      -- o elemento retira-o da lista e reconstroi a lista de alguidares 
removerDaListaDeAlguidaresAux x e n
  | n >= (length e) = e
  | x `elem` (e!!n) = removerDaListaDeAlguidaresAux x ((take (n) e) ++ [filter (\y -> not (y `elem` [x])) (e!!(n))] ++ (drop (n+1) e)) (n+1)
  |otherwise = removerDaListaDeAlguidaresAux x e (n+1)

delete :: Ord a => [[a]] -> [[a]]                                                                -- Elimina listas vazias que possam aparecer apos elementos terem sido removidos 
delete  []                 = []
delete (y:ys) | y == []    = delete ys
              | otherwise = y : delete ys

fromList :: Ord a => Int -> [a] -> [[a]]                                                         -- envia para o fromListAux uma lista ordenada para este poder dividir
fromList n xs = if(n>0) then fromListAux n (qsort xs) else error "Negative or zero n"

fromListAux :: Ord a => Int -> [a] -> [[a]]
fromListAux n xs = foldr (\x acc -> (take n xs) : (fromList n (drop n xs))) [] xs                -- divide em alguidares de tamanho n a lista xs semelhante a funcao divide 

mapListaDeAlguidares:: (Ord a, Ord b) => Int -> (a -> b) -> [[a]] -> [[b]]                       -- junta todos os alguidares aplica-lhes a funcao f organiza-os com o qsort e divide-os
mapListaDeAlguidares n f e = divide n (qsort (map f (concat e)))                                 -- em alguidares com o divide

createFastCache :: (Ord a, Ord b) => Int -> [a] -> [b] -> [[(a, b)]]                             -- junta ambas as listas com o zip, organiza-as com o qsort', e divideas com o 
createFastCache n xs xy = adicionaAListaDeAlguidaresCache n (qsort' (zip xs xy) )                -- adicionaAListaDeAlguidaresCache

qsort' :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]                                                   -- Semelhante ao qsort mas organiza pares de acordo com o primeiro elemento do par
qsort' [] = []
qsort' (x:xs) = qsort [a | a <- xs, (fst a) < (fst x)]
                      ++ [x] ++
               qsort [b | b <- xs, (fst b) >= (fst x)]

adicionaAListaDeAlguidaresCache :: (Ord a, Ord b) => Int -> [(a,b)] -> [[(a,b)]]                 -- Semelhante ao adicionaAListaDeAlguidaresAux mas trabalha sobre pares
adicionaAListaDeAlguidaresCache _ [] = []
adicionaAListaDeAlguidaresCache n xs
  | n > 0 = (take n xs) : (adicionaAListaDeAlguidaresCache n (drop n xs))
  | otherwise = error "Negative or zero n"


fastGet :: (Ord a, Ord b) => [[(a, b)]] -> a -> [b]                                              -- analiza as cabecas dos alguidares vendo se o valor da cabeca e menor que a, se for este
fastGet db n = foldl (\x acc -> if fst(acc!!0)<= n then x ++ (fastGetAux acc n) else x) [] db    -- o caso este e um algudar valido e este e enviado para o fastGetAux para os seus valores serem analizados

fastGetAux :: (Ord a, Ord b) =>[(a, b)] -> a -> [b]                                              -- Analiza os valores da lista e se o primeiro valor de algum par na lista for igual a n entao
fastGetAux db n = foldr (\x acc -> if (fst x == n) then (snd x):acc else acc) [] db              -- o segundo valor desse para sera adicionado ao acc


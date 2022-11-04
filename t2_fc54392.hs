paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos xs = [(xs!!a,xs!!(a+1)) | a <-[0 .. length xs-2]] 

diferencasConsecutivas :: [Int] -> [Int]
diferencasConsecutivas xs = [xs!!(a+1) - xs!!(a) | a <-[0 .. length xs-2]]


claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior (_,x,y) xs = claramentePiorAux (x,y) xs 

claramentePiorAux :: (Int, Int) -> [(String, Int, Int)] -> Bool
claramentePiorAux _ [] = False
claramentePiorAux a ((_,x2,y2):xs) 
    | (fst a < x2) && (snd a < y2) = True || (claramentePiorAux a xs)
    | xs == [] = False
    | otherwise = False || (claramentePiorAux a xs)

filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores xs = [ xs!!n | n <-[0 .. (length xs)-1] , filtroJogadoresAux xs!!n == False ]

filtroJogadoresAux :: [(String, Int, Int)] -> [Bool]
filtroJogadoresAux xs = [ claramentePior (xs!!a) xs | a <-[0 .. length xs-1] ]
    
preencherVazio :: [[Int]] -> Int
preencherVazio xs = preencherVazio'(xs!!(preencherVazioAux xs 0)) 1

preencherVazio' :: [Int] -> Int -> Int
preencherVazio' xs n
    | n >= 10 = 7
    | (n `elem` xs) == False = n
    | otherwise = preencherVazio' xs (n+1)

preencherVazioAux :: [[Int]] -> Int -> Int
preencherVazioAux [] _ = 0
preencherVazioAux (x:xs) n
    | 0 `elem` x = n
    |otherwise = preencherVazioAux xs (n+1)

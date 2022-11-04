frequencias :: [Char] -> [Int]
frequencias xs = [frequenciasAux xs [xs!!c]  | c <- [ 0 .. (length xs)-1] ]

frequenciasAux :: [Char] -> [Char] -> Int
frequenciasAux st v = length[ c | c <- st, c `elem` v]  

pequenasPalavras :: [String]
pequenasPalavras = [ [x] ++ [y] ++ [z] | x <-['a' .. 'z'] , y <-['a' .. 'z'] , z <-['a' .. 'z'] , (x `elem` ['a','e','i','o','u','y']) || (y `elem` ['a','e','i','o','u','y']) || (z `elem` ['a','e','i','o','u','y'])]

legendaCampainha :: Int -> Int -> [(String, Int)] -> [String]
legendaCampainha a n xs
    |   a == 0 = []
    |   a < n = [ show y ++ (fst(xs!!x)) |  y <-[1 .. a +1] , x <-[0 .. (length xs)-1],y /= n, y < (snd(xs!!x)+1)]
    |   otherwise = [ show y ++ (fst(xs!!x)) |  y <-[1 .. a +1] , x <-[0 .. (length xs)-1],y /= n, y <= (snd(xs!!x)+1)]
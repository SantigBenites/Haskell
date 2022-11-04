halve :: [a] -> ([a],[a])
halve xs = 
    let xl = take n xs
        xr = drop n xs
        n  = length xs `div` 2
    in (xl,xr)

halve' :: [a] -> ([a],[a])
halve' xs = 
    (take n xs , drop n xs)
    where n = length xs `div` 2


raizes :: Double -> Double -> Double -> (Double,Double)
raizes a b c = (raiz1 ,raiz2)
    where raiz1 = (b - sqrt(a*a - 4*a*c)) / 2  
          raiz2 = (b + sqrt(a*a - 4*a*c)) / 2

sum':: Num a =>[a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

substitui :: Eq a =>a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui a e (x:xs) 
    | x == a    = e : substitui a e xs
    | otherwise = x : substitui a e xs

posicoes :: [Int] -> Int -> [Int]
posicoes xs a = posicoesAux xs a 0

posicoesAux :: [Int] -> Int -> Int -> [Int]
posicoesAux [] _ _ = []
posicoesAux (x:xs) a b
    | x `mod` a == 0 = b:posicoesAux xs a (b+1)
    | otherwise = posicoesAux xs a (b+1)

repBinaria :: Int -> Int
repBinaria x 
    | x == 0 = 0
    | x == 1 = 1
    | mod x 2 == 0 = 10 * repBinaria(div x 2)
    | otherwise = 10 * repBinaria(div x 2) + 1

odioso :: Int -> Bool
odioso 0 = False
odioso 1 = True
odioso n
    | mod n 2 == 1 = odioso(div n 2)
    | otherwise = not(odioso(div n 2))
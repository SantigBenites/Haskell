--3

--(a) map (+1)[1..3] - [2,3,4]
--(b) map (>0)[3,-5,-2,0] - [True,False,False,False]
--(c) map (++"s")["A", "arte", "do", "aluno"] - ["As", "artes", "dos", "alunos"]
--(d) map ("s"++)["o", "aluno","bem-comportado"] - ["so", "saluno","sbem-comportado"]
--(e) let f x = x * x in map (map f)[[1,2],[3,4,5]] - [[1,4],[9,16,25]]
--(f) filter (>5)[1..6] - [6]
--(g) filter even [1..10] - [2,4,6,8,10]
--(h) filter (>0)(map (^2)[-3..3]) - [9,4,1,1,4,9]
--(i) map (^2)(filter (>0)[-3..3]) - [1,4,9]

zipWith' :: (a->b->c)-> [a] -> [b] -> [c]
zipWith' _ [] [] =  []
zipWith' _ _  [] =  []
zipWith' _ []  _ =  []
zipWith' f (s:xs) (y:xy) =  (f s y):(zipWith' f xs xy)

zipWith'' :: (a->b->c)-> [a] -> [b] -> [c]
zipWith'' f xs xy = [f (xs!!a) (xy!!a) | a<-[0 .. length xs-1]]

zip' :: [a] -> [b] -> [(a,b)]
zip' xs xy = let f x y = (x,y) in zipWith'' f xs xy

dropUntil :: (a -> Bool)-> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs)
    | f x == False = dropUntil f xs
    | otherwise = (x:xs)

total :: (Int -> Int)-> Int -> Int
total f n = sum(map f [0 .. n])


aplica :: [a -> a] -> [a] -> [a]
aplica [] xs = xs
aplica (f:fx) xs = aplica fx (map f xs)
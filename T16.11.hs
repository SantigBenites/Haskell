

reverse' :: [a] -> [a]
reverse' = rev []
    where
        rev acc (x:xs) = rev (x:acc) xs
        rev acc [] = acc  

-- Propriedades

-- A inversa da inversa e a lista original.

-- Uma lista e a sua inversa tem sempre o mesmo comprimento

prop_reverse_length :: [Int] -> Boold
prop_reverse_length xs = length xs == length (reverse' xs)
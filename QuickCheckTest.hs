import Test.QuickCheck

reverse' :: [a] -> [a]
reverse' xs = rev xs []
    where rev acc (x:xs) = rev (x:acc) acc
          rev acc []= acc 


prop_reverse_length :: [Int] -> Bool
prop_reverse_length xs = length (reverse' xs) == length xs

main :: IO()
main = do 
    quickCheck prop_reverse_length
writePrimes :: [Int] -> IO ()
writePrimes xs = putStrLn $ writePrimesAux' xs

writePrimesAux' :: [Int] -> String
writePrimesAux' [] = "\n"
writePrimesAux' (x:xs) = show(x)++"th" ++ " prime is " ++ show(primes!!x) ++ "\n" ++ (writePrimesAux' xs)

primes :: [Integer]
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


palindrome:: String -> Bool
palindrome xs = xs == reverse xs

palindromemain :: IO()
palindromemain = do
    linha <- getLine
    putStrLn $ if(palindrome linha) then "Sim" else "Nao"

palindromeContinuo :: IO()
palindromeContinuo = do
    putStrLn "tell me a word"
    linha <- getLine
    if null linha
        then return ()
        else do
            putStrLn $ if(palindrome linha) then "Sim" else "Nao"
            palindromeContinuo

palindromeContinuo2 :: IO()
palindromeContinuo2 = interact palindromeContinuo2Aux

palindromeContinuo2Aux :: String -> String
palindromeContinuo2Aux xs = unlines (map (\x ->if palindrome x then "Sim" else "Nao") (lines xs))

printEven :: Int -> IO()
printEven n = if(even n) then putStrLn "Par" else putStrLn "Impar"

showParity :: [Int] -> IO()
showParity [] = putStrLn " "
showParity (x:xs) = do
                printEven(x)
                showParity xs

showParity2 :: [Int] -> IO()
showParity2 xs = mapM_ printEven xs
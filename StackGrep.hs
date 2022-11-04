import System.Environment
import Data.List

main :: IO ()
main  = do 
    args <- getArgs  
    contents <- readFile (head args)
    putStrLn contents 
    putStrLn "Filtering:"
    analize contents []


analize ::String-> [String] -> IO ()
analize contents stack= do
    value <- getLine
    let linesOfFiles = lines contents
    if(value /= "pop")
        then do
            let newStack = stack ++ [value]
            putStrLn (unlines (containsBig linesOfFiles newStack))
            putStr "Filtering:"
            putStrLn $ show newStack
            analize contents newStack
        else do
            if(length stack == 0) 
                then return()
                else do
                let newStack = init stack
                putStrLn (unlines (containsBig linesOfFiles newStack))
                putStr "Filtering:"
                putStrLn $ show newStack
                analize contents newStack
        

containsBig :: [String] -> [String] -> [String]
containsBig [] _ = []
containsBig lines [] = lines
containsBig lines (x:xs) = containsBig (contains lines x) xs


contains :: [String] -> String -> [String]
contains [] _ = []
contains (x:xs) s = if(isInfixOf s x) then x:contains xs s else contains xs s 

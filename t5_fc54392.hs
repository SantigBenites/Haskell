import System.Environment
import System.IO
import Data.List

--Santiago Benites - fc54392 - 2020/21


main :: IO ()
main  = do 
    hSetBuffering stdout NoBuffering
    args <- getArgs  
    contents <- readFile (head args)
    putStrLn contents 
    putStrLn "Filtering: "
    putStr "> "
    analize contents []


analize ::String-> [String] -> IO ()
analize contents stack= do
    value <- getLine
    let linesOfFiles = lines contents
    if(value /= "pop")
        then do
            let newStack = stack ++ [value]
            putStrLn (unlines (containsBig linesOfFiles newStack))
            putStr "Filtering: "
            putStrLn $ list_to_string $ newStack
            putStr "> "
            analize contents newStack
        else do
            if(length stack == 0) 
                then return()
                else do
                let newStack = init stack
                putStrLn (unlines (containsBig linesOfFiles newStack))
                putStr "Filtering: "
                putStrLn $ list_to_string $ newStack
                putStr "> "
                analize contents newStack
        
list_to_string :: [String] -> String
list_to_string xs =  Data.List.intercalate ", " $ Data.List.map (\x -> id x) xs

containsBig :: [String] -> [String] -> [String]
containsBig [] _ = []
containsBig lines [] = lines
containsBig lines (x:xs) = containsBig (contains lines x) xs


contains :: [String] -> String -> [String]
contains [] _ = []
contains (x:xs) s = if(isInfixOf s x) then x:contains xs s else contains xs s 

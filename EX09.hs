import System.Environment
import System.IO
import Data.List

toFile :: Show a => FilePath -> [a] -> IO ()
toFile filePath xs = do
    handle <- openFile filePath WriteMode
    hPutStr handle $ unlines $ map show xs
    hClose handle

toFile' :: Show a => FilePath -> [a] -> IO ()
toFile' filePath xs = writeFile filePath $ unlines $ map show xs

toFile'' :: Show a => FilePath -> [a] -> IO ()
toFile'' filePath xs = withFile filePath WriteMode (\handle -> hPutStr handle $ unlines $ map show xs)

fromFile :: Read a => FilePath -> IO [a]
fromFile filePath = do
    contents <- readFile filePath
    return $ map read $ lines contents

filterFiles :: (String -> Bool) -> FilePath -> FilePath -> IO()
filterFiles funcao filePath1 filePath2 = do
    contents <- readFile filePath1
    let linhasValidas = filter funcao (lines contents)
    toFile filePath2 linhasValidas

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (_:_) [] = False
isPrefix (x:xs) (y:ys) = x==y && isPrefix xs ys


filterPrefix :: String -> FilePath -> FilePath -> IO ()
filterPrefix = filterFiles . isPrefix

main5 :: IO()
main5 = do
    [prefixo,filePath1,filePath2] <- getArgs
    filterPrefix prefixo filePath1 filePath2


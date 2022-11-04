import System.IO
import System.Environment
import Data.List
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs 
    let list = []
    if( "-t" `elem` args) 
        then do tests
        else do line <- getLine
                let sumLine = tail(split ' ' line)
                let columnNumber = read(head sumLine) :: Int
                let listForFun = wordRemove (tail(sumLine)) "groupby"
                if("sum " `isInfixOf` line)then do sumFun list columnNumber listForFun
                    else if("average " `isInfixOf` line)then do averageFun list columnNumber listForFun
                        else if("maximum " `isInfixOf` line)then do maxFun list columnNumber listForFun
                            else return()

sumFun :: [String] -> Int -> [String] -> IO()
sumFun xs n c = do
    line <- getLine
    if(line == "exit")
        then return()
        else do
            let newList = line:xs
            let lineList = split ' ' line
            let restrictions = makeConstrictions c lineList
            let restrictedList = constrictions newList restrictions
            putStrLn $ show(sum(makeList restrictedList n))
            sumFun newList n c

averageFun :: [String] -> Int -> [String] -> IO()
averageFun xs n c = do
    line <- getLine
    if(line == "exit")
        then return()
        else do
            let newList = line:xs
            let lineList = split ' ' line
            let restrictions = makeConstrictions c lineList
            let restrictedList = constrictions newList restrictions
            putStrLn $ show(average(makeList restrictedList n))
            averageFun newList n c

maxFun :: [String] -> Int -> [String] -> IO()
maxFun xs n c = do
    line <- getLine
    if(line == "exit")
        then return()
        else do
            let newList = line:xs
                lineList = split ' ' line
                restrictions = makeConstrictions c lineList
                restrictedList = constrictions newList restrictions
            putStrLn $ show(maximum(makeList restrictedList n))
            maxFun newList n c

average :: [Float] -> Float
average [] = 0
average xs = sum xs / fromIntegral (length xs)

makeList :: [String] -> Int -> [Float]
makeList [] _ = []
makeList (x:xs) n = (read ((split ' ' x)!!n)):makeList xs n

makeConstrictions :: [String] -> [String] -> [(Int,Float)]
makeConstrictions _ [] = []
makeConstrictions [] _ = []
makeConstrictions (x:xs) xy = 
    let value = read x
    in (value, read(xy!!value)):makeConstrictions xs xy

constrictions :: [String] -> [(Int,Float)] -> [String]
constrictions [] _ = []
constrictions xs [] = xs
constrictions xs (x:c) = constrictions (restriction xs x) c


restriction :: [String] -> (Int,Float) -> [String]
restriction [] _ = []
restriction (x:xs) (a,b) 
    | a > length list = x:xs
    | read(list !! a) == b = x:(restriction xs (a,b))
    | otherwise = restriction xs (a,b)
    where list = split ' ' x

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split separator ys = f : (split separator (dropSeparator separator rest))
  where (f, rest) = break (== separator) ys

dropSeparator :: Eq a => a ->  [a] -> [a]
dropSeparator _ [] = []
dropSeparator separator (x:xs) = if x == separator then xs else x:xs

wordRemove :: (Eq a) => [a] -> a -> [a]
wordRemove [] _ = []
wordRemove xs w = [x | x <- xs, x /= w]

prop_wordRemove_length :: String -> [String] -> Bool
prop_wordRemove_length x xs = length (wordRemove xs x) <= length xs

prop_wordRemove_associative :: String -> String -> [String] -> Bool
prop_wordRemove_associative x y xs = wordRemove (wordRemove xs x) y == wordRemove (wordRemove xs y) x

prop_dropSeparator_length :: String -> [String] -> Bool
prop_dropSeparator_length x xs = length (dropSeparator x xs) <= length xs 

prop_dropSeparator_associative :: String -> String -> [String] -> Bool
prop_dropSeparator_associative x y xs = dropSeparator y (dropSeparator x xs) == dropSeparator x (dropSeparator y xs)

prop_average_sum :: [Float] -> Bool
prop_average_sum xs = average xs <= average (map (+1) xs)

tests :: IO ()
tests = do 
    quickCheck prop_wordRemove_length
    quickCheck prop_wordRemove_associative
    quickCheck prop_dropSeparator_length
    quickCheck prop_dropSeparator_associative
    quickCheck prop_average_sum

    
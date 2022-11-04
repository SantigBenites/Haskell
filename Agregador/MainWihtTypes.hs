import System.IO
import System.Environment
import Test.QuickCheck
import Control.Monad (unless)

data Metric = Metric { func :: Type  
                     , values :: [String]  
                     , n :: Int  
                     , constrict :: [String]
                     }

data Type = Type { funcName ::  String ,
                   functionExe :: ([Float] -> Float)   }

instance Show Type where
    show (Type n e) = n

instance Show Metric where
    show (Metric f p a xs) = show f ++ " " ++ Prelude.show p ++ " " ++ Prelude.show a

rType :: Gen Type
rType = elements [maximumType,averageType,sumType]

rMetric :: Gen Metric
rMetric = do 
           r <- rType
           value <- (choose (0,5) :: Gen Int)
           return (Metric r ["0 1","0 1","0 1","0 1","0 1","0 1"] value [])

maximumType :: Type
maximumType = Type {funcName = "maximum", functionExe = maximum}

averageType :: Type
averageType = Type {funcName = "average", functionExe = average}

sumType :: Type
sumType = Type {funcName = "sum", functionExe = sum}

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering                
    testCheck <- getArgs
    if( "-t" `elem` testCheck) 
        then do tests
        else do                                         -- ex:
        line <- getLine                                 -- sum 1 groupby 2
        let input = words line                          -- ["sum", "1", "groupby", "2", "groupby", "4"]
            function = getF $ head input                -- sum
            args = tail input                           -- ["1", "groupby", "2", "groupby", "4"]
            columnNumber = read (head args) :: Int      -- 1
            list = filter ("groupby" /=) (tail args)    -- ["2", "4"]
        applyMetrics function [] columnNumber list

applyMetrics :: ([Float] -> Float) -> [String] -> Int -> [String] -> IO ()
applyMetrics f xs n c = do
    line <- getLine
    unless (line == "exit") $ do
        let newList = line:xs
            lineList = words line
            restrictions = makeConstrictions c lineList
            restrictedList = constrictions newList restrictions
        print $ f (makeList restrictedList n)
        applyMetrics f newList n c

makeList :: [String] -> Int -> [Float]
makeList [] _ = []
makeList (x:xs) n = read (words x!!n) : makeList xs n

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
    where list = words x

getF :: String -> [Float] -> Float
getF "maximum" = maximum
getF "sum" = sum
getF "average" = average

average :: [Float] -> Float
average [] = 0
average xs = sum xs / fromIntegral (length xs)

tests :: IO ()
tests = do 
        quickCheck prop_average_sum
        quickCheck prop_average_between

prop_average_sum :: [Float] -> Bool
prop_average_sum xs = average xs <= average (map (+1) xs)

prop_average_between :: [Float] -> Bool
prop_average_between [] = True
prop_average_between xs = ((minimum xs) <= (average xs) ) && ( (average xs) <= (maximum xs))

--prop_getF :: Metric -> Bool
--prop_getF (Metric f xs n) = (applyMetrics f xs n []) ==
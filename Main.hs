module Main (main) where
import System.IO
import System.Environment
import Test.QuickCheck
import Control.Monad (unless)
import Data.List ( transpose )

{-|
    @author Santiago Benites fc54392
    @author Joao Ferreira fc55312
-}

type Input = [Float]
type Value = Float

-- Data Types used to test the code
-- Creates a valid Float Array that represents various liens of the users input
data ValidInputArray = ValidInputArray { list :: [Input]} deriving Show
-- Creates a single line of the users input
data ValidInput = ValidInput { array :: Input} deriving Show
-- Creates a Int that can be used as a a valid position of the input
-- Used to reduce the number of discarded cases
data ValidInt = ValidInt { number :: Int} deriving Show

-- Arbitrary relative to ValidInput, creates a data type with a [Float]
instance Arbitrary ValidInput where
    arbitrary = do
                value <- (choose (1,10) :: Gen Int)
                r <- rInput value
                return (ValidInput r)

-- Arbitrary relative to ValidInputArray, creates a data type with a [Input]
instance Arbitrary ValidInputArray where
    arbitrary = do
                value <- (choose (1,10) :: Gen Int)
                size <- (choose (1,10) :: Gen Int)
                r <- vectorOf size (rInput value)
                return (ValidInputArray r)

--Arbitrary relative to ValidInt, creates a data type with a Int
instance Arbitrary ValidInt where
    arbitrary = do
                r <- (choose (1,5) :: Gen Int)
                return (ValidInt r)

-- Function that returns a Generator of valid inputs
-- Basicly a [Float] with length n where all the floats 
-- have been truncated to 2 decimal places and are positive
rInput :: Int -> Gen Input
rInput n = do 
           r <- vectorOf n (arbitrary :: Gen Float)
           return (fmap abs (fmap truncate' r))

-- truncates a float to 2 decimal places
truncate' :: Float -> Float
truncate' x = (fromIntegral (floor (x * t))) / t
    where t = 10^2

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering                
    testCheck <- getArgs
    if( "-t" `elem` testCheck) 
        then do tests
        else do                                               -- ex:
        line <- getLine                                       -- sum 1 groupby 2
        let input = words line                                -- ["sum", "1", "groupby", "2", "groupby", "4"]
            function = getF $ head input                      -- sum
            args = tail input                                 -- ["1", "groupby", "2", "groupby", "4"]
            columnNumber = read (head args) :: Int            -- 1
            groupby = s2I $ filter ("groupby" /=) (tail args) -- ["2", "4"]
        applyMetrics function [] columnNumber groupby

{-| applyMetrics receives the input from the user and applies the 
    metric to it and the stored memory printing the result to stOut
    Function receives as input:
    f - Function that will be aplied to the 
        collumn of the restrictedList
    xs- Matrix that represents past inputs
    n - Collum of xs we want to input into f
    c - Restricitons given by group by in Main
-}
applyMetrics :: ([Float] -> Float) -> [Input] -> Int -> [Int] -> IO ()
applyMetrics f xs n c = do
    line <- getLine
    unless (line == "exit") $ do
        let input = s2f $ words line
            memory = input:xs
            restrictions = makeConstrictions c input
            restrictedList = constrictions memory restrictions
        print $ f (makeList restrictedList n)
        applyMetrics f memory n c

{-| Receives matrix and returns a collumn in list
-}
makeList :: [Input] -> Int -> [Float]
makeList [] _ = []
makeList xs i = transpose xs !! i

{-| Receives list of ints and input and returns a pair, where the first element is 
    the int and the second is the value in the position int in the input.
-}
makeConstrictions :: [Int] -> Input -> [(Int,Float)] 
makeConstrictions _ [] = []
makeConstrictions [] _ = []
makeConstrictions (x:xs) xy = (x, xy!!x) : makeConstrictions xs xy

{-| Applies restriction to a input and a pair (Int,Float) and 
    takes that input and aplies it to the next member of the [(Int,Float)] list
-}
constrictions :: [Input] -> [(Int,Float)] -> [Input]
constrictions [] _ = []
constrictions xs [] = xs
constrictions xs (x:c) = constrictions (restriction xs x) c

{-| Recieves a List of inputs and a pair type (Int,Float) and returns a list of 
    all the inputs of the list where the position int in the list is equal to the float.
-}
restriction :: [Input] -> (Int,Float) -> [Input]
restriction [] _ = []
restriction (x:xs) (a,b)
    | a > length x = x:xs
    | x !! a == b = x:restriction xs (a,b)
    | otherwise = restriction xs (a,b)

{-| Recieves a String and a List of Floats and applies the 
    function represented by the string to the float list
-}
getF :: String -> Input -> Float
getF "maximum" = maximum
getF "sum"     = sum
getF "average" = average

average :: [Float] -> Float
average [] = 0
average xs = sum xs / fromIntegral (length xs)

s2f :: [String] -> Input
s2f = map (read::String->Float)

s2I :: [String] -> [Int]
s2I = map (read::String->Int)

{-| Executes all the tests
-}
tests :: IO ()
tests = do 
        quickCheck prop_average_sum
        quickCheck prop_restriction_length
        quickCheck prop_makeList_loss

{-| Tests to see if adding 1 to all members of the input list and checks if the 
    average of the new list is larger that the original one
-}
prop_average_sum :: ValidInput -> Bool
prop_average_sum (ValidInput n) = average n <= average (map (+1) n)

minimumLength :: [[a]] -> Int -> Int
minimumLength [] acc = acc
minimumLength (x:xs) acc 
    | acc == 0 = minimumLength xs (length x)
    | acc > length x = minimumLength xs (length x)
    |otherwise = minimumLength xs acc

{-| Tests to see if the length of the list after restrictions 
    being applied is smaller or equal than it was before
-}
prop_restriction_length :: ValidInputArray -> ValidInt -> Float -> Property
prop_restriction_length (ValidInputArray n) (ValidInt x) y = 
    (x < minimumLength n 0) ==> length (restriction n (x,y)) <= length n

{-| Tests to see if the length of the list before being applied 
    makeList is equal to after it as been applied
-}
prop_makeList_loss :: ValidInputArray -> ValidInt -> Property
prop_makeList_loss (ValidInputArray xs) (ValidInt n) = 
    (n < minimumLength xs 0) ==> length (makeList xs n) == length xs

data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show, Read)
instance Eq (a) => Eq (Tree a) where  
    EmptyTree == EmptyTree = True  
    (Node lefta a righta) == (Node leftb b rightb) = (lefta==leftb)&&(a==b)&&(righta==rightb)  
    _ == _ = False  


empty :: Tree a
empty = EmptyTree

size :: Tree a -> Int
size EmptyTree = 0
size (Node left _ right) = 1 + size left + size right

depth :: Tree a -> Int
depth EmptyTree = 0
depth (Node left _ right) = 1 + max (depth left)  (depth right)

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten (Node left a right) = [a] ++ flatten left ++ flatten right

isPerfect :: (Eq a) => Tree a -> Bool
isPerfect EmptyTree = True
isPerfect (Node EmptyTree _ (Node _ _ _)) = False
isPerfect (Node (Node _ _ _) _ EmptyTree) = False
isPerfect (Node left _ right) = (depth left == depth right)&&(isPerfect left)&&(isPerfect right)

isPerfect' :: Tree a -> Bool
isPerfect' = fst . isPerfectAux'

isPerfectAux':: Tree a -> (Bool,Int)
isPerfectAux' EmptyTree = (True,0)
isPerfectAux' (Node left _ right) = (b1 && b2 && s1 == s2, s1 + s2 +1)
    where (b1,s1)  = isPerfectAux' left
          (b2,s2) = isPerfectAux' right

invert :: Tree a -> Tree a
invert EmptyTree = EmptyTree
invert (Node left x right) = Node (invert right) x (invert left)

makeTree :: [a] -> Tree a
makeTree [] = EmptyTree
makeTree (x:xs) =  Node (makeTree xls) x (makeTree xrs )
    where n = length xs `div` 2
          xls = take n xs
          xrs = drop n xs

isIn :: Eq a =>a -> Tree a -> Bool
isIn _ EmptyTree = False
isIn x (Node left a right) = (x == a)||(isIn x left)||(isIn x right)

allIn :: Eq a =>Tree a -> Tree a -> Bool 
allIn EmptyTree EmptyTree = True
allIn (Node _ _ _) EmptyTree = False
allIn EmptyTree (Node _ _ _) = False
allIn (Node l x r) tree = isIn x tree && allIn l tree && allIn r tree

t1 :: (Num a)=> Tree a
t1 = (Node (Node ((Node (EmptyTree) 1 (EmptyTree))) 2 (Node (EmptyTree) 3 (EmptyTree))) 4 (Node (Node (EmptyTree) 5 (EmptyTree)) 6 (Node (EmptyTree) 7 (EmptyTree))))
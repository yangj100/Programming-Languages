import Data.ByteString (count)
import Text.XHtml (treeColors)
fib :: Integer -> Integer
fib i | i == 0 = 0
      | i == 1 = 1
      | i >= 2 = fib (i - 1) + fib (i - 2)

matchEnds :: String -> Bool
matchEnds []    = False
matchEnds [x]   = False
matchEnds (x:xs)= x == last xs 

filterDigits :: [Integer] -> [Integer]
filterDigits [] = []
filterDigits (x:xs) | x >= 0 && x <= 9 = x : filterDigits xs
                    | otherwise = filterDigits xs

------------End of Quiz 1--------------

findMin :: (Ord a) => [a] -> a
findMin [] = error "Error"
findMin x = minimum x

--I keep messing up the syntax but you basically got it down.
delete :: (Eq a) => a -> [a] -> [a]
delete _ [] = []
delete x (l:xs) | x == l = xs
                | otherwise = l : delete x xs
{-
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort l = 
             let mx = findMin l
             xs = delete mx l
             in mx : selectionSort xs
-}
indexHelper :: Integer -> [a] -> [(Integer,a)]
indexHelper _ [] = []
indexHelper n (x:xs) = (n,x) : indexHelper (n+1) xs

index :: [a] -> [(Integer, a)]
index = indexHelper 0 


reverse2x :: [String] -> [String]
reverse2x xs = reverse(map reverse xs)

countEmpty :: [String] -> Integer
countEmpty [] = 0
countEmpty (x:xs) = if x == "" then 1 else 0 + countEmpty xs


takeWith :: [String] -> [Int] -> [String]
takeWith [] _ = []
takeWith _ [] = []
takeWith (s:ss) (x:xs) = take x s : takeWith ss xs

findOdd :: [Integer] -> Maybe Integer
findOdd = foldl (\acc x -> if acc == Nothing && odd x then Just x else acc)Nothing

data MTree a 
    = MLeaf a 
    | UNode a (MTree a) 
    | BNode (MTree a) (MTree a)
    deriving (Eq, Show)

myTree :: MTree Integer
myTree = BNode 
    (UNode 7 (MLeaf 3)) 
    (UNode 5 
        (BNode 
            (UNode 3 (MLeaf 2)) 
            (MLeaf 10)
        )
    )
    

countUNodes :: MTree a -> Integer
countUNodes (MLeaf _ ) = 0
countUNodes (UNode _ tree ) = 1 + countUNodes tree
countUNodes (BNode l r) = countUNodes l + countUNodes r 

mulMLeaves :: MTree Integer -> Integer 
mulMLeaves (MLeaf x) = x
mulMLeaves (UNode _ tree) = mulMLeaves tree
mulMLeaves (BNode l r) = mulMLeaves l * mulMLeaves r

allPred :: (a -> Bool) -> MTree a -> Bool
allPred _ (MLeaf x) = x `allPred`


{-
maxSumBranch :: MTree Integer -> [Integer]
maxSumBranch (MLeaf x) = [x]
maxSumBranch (UNode x tree) = x : maxSumBranch tree
maxSumBranch (BNode l r) = 
    let left = maxSumBranch l
        right = maxSumBranch r
    in if sum left >= sum right 
        then left
        else right

-}

foldMTree :: (b -> b -> b) -> (a -> b -> b) -> (a -> b) -> MTree a -> b
foldMTree n u l (MLeaf x) = l x
foldMTree n u l (UNode x t) = u x (foldMTree n u l t)
foldMTree n u l (BNode t1 t2) = n (foldMTree n u l t1) (foldMTree n u l t2)

maxSumBranch :: MTree Integer -> [Integer] 
maxSumBranch = foldMTree maxSumBranch sumBranch leafBranch
    where 
        leafBranch x = [x]
        sumBranch x left = x:left
        maxBranchSum leftSum rightSum = if sum leftsum >= sum rightsum then leftSum else rightSum
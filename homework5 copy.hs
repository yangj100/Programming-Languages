data LTree a = LLeaf a | LNode a (LTree a) (LTree a) deriving (Eq, Show)
data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a) deriving (Eq, Show)

getLLeaves :: LTree a -> [a]
getLLeaves (LLeaf x) = [x]
getLLeaves (LNode _ l r) = getLLeaves l ++ getLLeaves r

getMLeaves :: MTree a -> [a]
getMLeaves (MLeaf x) = [x]
getMLeaves (UNode _ t) = getMLeaves t
getMLeaves (BNode l r) = getMLeaves l ++ getMLeaves r

maxLDepth :: LTree a -> Integer
maxLDepth (LLeaf _) = 0
maxLDepth (LNode _ l r) = 1 + max (maxLDepth l) (maxLDepth r)

maxMDepth :: MTree a -> Integer
maxMDepth (MLeaf _) = 0
maxMDepth (UNode _ t) = 1 + maxMDepth t
maxMDepth (BNode l r) = 1 + max (maxMDepth l) (maxMDepth r)

maxLTree :: LTree Integer -> Integer
maxLTree (LLeaf x) = x
maxLTree (LNode x l r) = max x (max (maxLTree l) (maxLTree r))

maxMTree :: MTree Integer -> Integer
maxMTree (MLeaf x) = x
maxMTree (UNode x t) = max x (maxMTree t)
maxMTree (BNode l r) = max (maxMTree l) (maxMTree r)

uncoveredLeafL :: Integer -> LTree Integer -> Bool
uncoveredLeafL val (LLeaf x) = x == val
uncoveredLeafL val (LNode x l r) = (x /= val && uncoveredLeafL val l) || (x /= val && uncoveredLeafL val r)

uncoveredLeafM :: Integer -> MTree Integer -> Bool
uncoveredLeafM val (MLeaf x) = x == val
uncoveredLeafM val (UNode x t) = x /= val && uncoveredLeafM val t
uncoveredLeafM val (BNode l r) = uncoveredLeafM val l || uncoveredLeafM val r

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (LLeaf x) = LLeaf (f x)
mapLTree f (LNode x l r) = LNode (f x) (mapLTree f l) (mapLTree f r)

mapMTree :: (a -> b) -> MTree a -> MTree b
mapMTree f (MLeaf x) = MLeaf (f x)
mapMTree f (UNode x t) = UNode (f x) (mapMTree f t)
mapMTree f (BNode l r) = BNode (mapMTree f l) (mapMTree f r)

applyLfun :: LTree Integer -> LTree Integer
applyLfun = mapLTree (\x -> 2 * x^2 - x^2)

applyMfun :: MTree Integer -> MTree Integer
applyMfun = mapMTree (\x -> 2 * x^2 - x^2)

orMaybes :: Maybe a -> Maybe a -> Maybe a
orMaybes (Just x) _ = Just x
orMaybes Nothing y = y

findLTree :: (a -> Bool) -> LTree a -> Maybe a
findLTree p (LLeaf x) = if p x then Just x else Nothing
findLTree p (LNode x l r) = if p x then Just x else orMaybes (findLTree p l) (findLTree p r)

findMTree :: (a -> Bool) -> MTree a -> Maybe a
findMTree p (MLeaf x) = if p x then Just x else Nothing
findMTree p (UNode x t) = if p x then Just x else findMTree p t
findMTree p (BNode l r) = orMaybes (findMTree p l) (findMTree p r)

findLpali :: LTree String -> Maybe String
findLpali = findLTree (\s -> s == reverse s)

findMpali :: MTree String -> Maybe String
findMpali = findMTree (\s -> s == reverse s)

foldLTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldLTree _ l (LLeaf x) = l x
foldLTree n l (LNode x t1 t2) = n x (foldLTree n l t1) (foldLTree n l t2)

foldMTree :: (b -> b -> b) -> (a -> b -> b) -> (a -> b) -> MTree a -> b
foldMTree _ _ l (MLeaf x) = l x
foldMTree n u l (UNode x t) = u x (foldMTree n u l t)
foldMTree n u l (BNode t1 t2) = n (foldMTree n u l t1) (foldMTree n u l t2)

getLLeaves' :: LTree a -> [a]
getLLeaves' = foldLTree (\_ l r -> l ++ r) (: [])

getMLeaves' :: MTree a -> [a]
getMLeaves' = foldMTree (++) (\_ t -> t) (: [])

uncoveredLeafL' :: Integer -> LTree Integer -> Bool
uncoveredLeafL' val = foldLTree (\x l r -> (x /= val && l) || (x /= val && r)) (== val)

uncoveredLeafM' :: Integer -> MTree Integer -> Bool
uncoveredLeafM' val = foldMTree (||) (\x t -> x /= val && t) (== val)

exLTree :: LTree Integer
exLTree = LNode 5 (LLeaf 4)
              (LNode 3 (LNode 2 (LLeaf 5) (LLeaf 1))
                       (LLeaf 7))

exMTree :: MTree Integer
exMTree = BNode (UNode 5 (BNode (UNode 1 (MLeaf 1)) (MLeaf 10)))
                (BNode (UNode 3 (MLeaf 3)) (UNode 4 (MLeaf 3)))

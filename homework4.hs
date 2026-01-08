mapPair :: (a -> b -> c) -> [(a, b)] -> [c]
mapPair x xs = map (uncurry x) xs




mapPair f xs = map (uncurry f) xs

mapPair' :: (a -> b -> c) -> [(b, a)] -> [c]
mapPair' x = map (\(b, a) -> x b a b)



mapPair' f = map (\(b, a) -> f a b)

digitsOnly :: [Int] -> [Int]
digitsOnly x = filter(\x -> x >= 0 && x <= 9) x


digitsOnly = filter(\x -> x >= 0 && x <= 9) 

removeXs :: [String] -> [String]
removeXs = filter(\x -> null x || head vx /= 'X') 


removeXs = filter(\x -> null x || head x /= 'X')

sqLens :: [String] -> [Integer]
sqLens = map(\s -> fromIntegral(length s) ^ 2)



sqLens = map (\s -> (fromIntegral (length s) ^ 2))

bang :: [String] -> [String]
bang = map(++'!')



bang = map (++ "!")

diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (-)



diff = zipWith (-)

splice :: [String] -> [String] -> [String]




splice q r = zipWith (\x y -> x ++ y ++ x) q r

firstStop :: String -> String




firstStop = takeWhile(/= '.')

boundRange :: Integer -> [Integer] -> [Integer]




boundRange n = takeWhile(\x -> x >= -n && x <= n) 

-- 1. exists
exists :: (a -> Bool) -> [a] -> Bool




exists _ [] = False
exists f (x:xs) = f x || exists f xs

exists' :: (a -> Bool) -> [a] -> Bool




exists' z y = foldl (\acc x -> if z x then True else acc) False y

-- 2. noDups
noDups :: Eq a => [a] -> [a]




noDups [] = []
noDups (x:xs)
  | x `elem` xs = x : noDups (filter (/= x) xs)
  | otherwise   = x : noDups xs

noDups' :: Eq a => [a] -> [a]
[]



noDups' = foldl (\acc x -> if x `elem` acc then acc else acc ++[x]) []

-- 3. countOverflow
countOverflow :: Integer -> [String] -> Integer




countOverflow _ [] = 0
countOverflow n (x:xs)   | fromIntegral (length x) > n = 1 + countOverflow n xs
                         | otherwise                   = countOverflow n xs

countOverflow' :: Integer -> [String] -> Integer




countOverflow' n = foldl (\acc x -> if fromIntegral (length x) > n then acc + 1 else acc) 0

-- 4. concatList
concatList :: [[a]] -> [a]




concatList [] = []
concatList (x:xs) = x ++ concatList xs






concatList'  = foldr (++) []

-- 5. bindList
bindList :: (a -> [b]) -> [a] -> [b]




bindList _ [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]




bindList' f = foldr (\x acc -> f x ++ acc) []

-- == Lecture 7 ================================================================
-- 1.1
takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

-- 1.2
index :: [a] -> [(Int, a)]
index = zip [0..]

index' :: [a] -> [(a, Int)]
index' = (`zip` [0..])

-- 1.3
divider :: Int -> String
divider = (`replicate` '=')

-- 2.1
applyOnLast f xs ys = f (last xs) (last ys)

addThree x y z = x + y + z
lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = addThree 100 (last xs) (last ys)

-- 2.2
applyManyTimes n f x
  | n <= 0    = x
  | otherwise = applyManyTimes (n - 1) f (f x)

applyTwice = applyManyTimes 2

-- 3.1
listifylist :: [a] -> [[a]]
listifylist = map (:[])

-- 3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (min 100)

-- 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- 4.2
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

-- 4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\y -> (freq y xs) == n) xs

-- 5.1
withinInterval n m = filter (\x -> x >= n && x <= m)

-- 5.2
sndColumn :: [[a]] -> [a]
sndColumn = map (\x -> x !! 1)

-- 5.3
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (\x -> (min (fst x) (snd x), max (fst x) (snd x))) $ filter (\(x, y) -> x /= y) xs

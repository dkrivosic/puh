import Data.Char
import Data.List

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

-- == Lecture 8 ================================================================
-- 1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter ((==0) . (`mod` 2) . fst) . zip [0..]

-- 1.2
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- 1.3
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concatMap ((:d) . toUpper . head) . filter p . words

-- 2.1
maxDiff :: [Int] -> Int
maxDiff xs =  maximum $ map (\x -> abs((fst x) - (snd x))) $ zip xs (tail xs)

maxDiff' :: [Int] -> (Int, Int)
maxDiff' xs =  (minimum diffs, maximum diffs)
  where diffs = map abs $ zipWith (-) xs $ tail xs

-- 2.2
studentsPassed s = map fst $ filter ((>=m) . snd) s
  where m =  (maximum $ map snd s) / 2

-- 3.1
isTitleCased :: String -> Bool
isTitleCased = all (`elem` ['A'..'Z']) . map head . words


-- 3.2
sortPairs :: Ord b => [(a, b)] -> [(a, b)]
sortPairs = sortBy compareSnd
  where compareSnd x y = (snd x) `compare` (snd y)

-- 3.3
filename :: String -> String
filename s = map snd $ dropWhile ((<=i) . fst) $ zip [0..] s
  where i = last $ findIndices (=='/') s

-- 3.4
maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = findIndices (==m) xs
  where m = maximum xs

-- 4.1
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> (y == x) || acc) False

-- 4.2
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- 4.3
nubRuns :: Eq a => [a] -> [a]
nubRuns xs = foldr fun [] xs
  where fun x []  = [x]
        fun x acc = if x == (head acc) then acc else x : acc

-- 5.1
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- 5.2
sumEven' :: [Integer] -> Integer
sumEven' = foldl (+) 0 . map snd . filter ((==0) . (`mod` 2) . fst) . zip [0..]

-- 5.3
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip xs = foldl maxPair (head xs) xs
  where maxPair (z1, z2) (x, y) = ((max z1 x), (max z2 y))

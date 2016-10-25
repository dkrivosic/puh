import Data.List

-- 1.a
isWellFormed ::  [[Int]] -> Bool
isWellFormed [[]] = False
isWellFormed xss = and [length xs == len | xs <- xss]
  where len = length $ xss !! 0

-- 1.b
size ::  [[Int]] -> (Int, Int)
size xss
  | not $ isWellFormed xss = error "Matrix is malformed"
  | otherwise              = (n, m)
  where n = length xss
        m = length $ xss !! 0

-- helper function
wrongIndex :: Int -> Int -> Bool
wrongIndex index size = (index >= size) || (index < 0)

-- 1.c
getElement ::  [[Int]] -> Int -> Int -> Int
getElement xss i j
  | wrongIndex i n = error "Index out of bounds"
  | wrongIndex j m = error "Index out of bounds"
  | otherwise      = xss !! i !! j
  where (n, m) = size xss

-- 1.d
getRow :: [[Int]] -> Int -> [Int]
getRow xss i
  | wrongIndex i n = error "Index out of bounds"
  | otherwise      = xss !! i
  where (n, _) = size xss

-- 1.e
getCol :: [[Int]] -> Int -> [Int]
getCol xss i
  | wrongIndex i m = error "Index out of bounds"
  | otherwise      = [xs !! i | xs <- xss]
  where (_, m) = size xss

-- 1.f
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss
  | (nx /= ny) || (mx /= my) = error "Matrices are not of equal size"
  | otherwise                = [[x + y | (x, y) <- zip xs ys] | (xs, ys) <- zip xss yss]
  where (nx, mx) = size xss
        (ny, my) = size yss

-- 1.g
transpose' :: [[Int]] -> [[Int]]
transpose' xss = [getCol xss i | i <- [0..(m-1)]]
  where (_, m) = size xss

-- 1.h
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xss yss
  | mx /= ny  = error "Incompatible matrix dimensions"
  | otherwise = [[sum [x * y | (x, y) <- zip xs ys] | ys <- transpose' yss] | xs <- xss]
  where (_, mx) = size xss
        (ny, _) = size yss

-- 2
type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value, Int)]

-- 2.a
exists :: Key -> Dictionary -> Bool
exists key dict = sum [1 | (k, _) <- dict, k == key] > 0

-- 2.b
get :: Dictionary -> Key -> Value
get dict key
  | not $ exists key dict = error $ "key " ++ show key ++ " not found"
  | otherwise           = head [v | (k, v) <- dict, k == key]

-- 2.c
insert :: Entry -> Dictionary -> Dictionary
insert (key, val) dict
  | exists key dict = [if key == k then (k, val) else (k, v) | (k, v) <- dict]
  | otherwise       = dict ++ [(key, val)]

-- 2.d
delete :: Key -> Dictionary -> Dictionary
delete key dict = [(k, v) | (k, v) <- dict, k /= key]

-- 2.e
freq :: Dictionary -> Frequency
freq [] = error "dictionary is empty"
freq dict = nub [(v1, sum [1 | (_, v2) <- dict, v1 == v2])| (_, v1) <- dict]

-- 3
largestMultiple ::  String -> Int
largestMultiple n
  | null ls   = error "No such number"
  | otherwise = head ls
  where ls = reverse $ sort [read x :: Int | x <- permutations n, (read x :: Int) `mod` 30 == 0]

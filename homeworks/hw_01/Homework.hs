import Data.Char

-- 1.a
norm v = sqrt $ (fst v)^2 + (snd v)^2

-- 1.b
normalize v
  | v == (0, 0) = error "Cannot normalize null vector"
  | otherwise = (fst v / norm v, snd v / norm v)

-- 1.c
scalarMult a (x1, x2) = (x1 * a, x2 * a)

-- 1.d
dot (x1, x2) (y1, y2) = x1 * y1 + x2 * y2

-- 1.e
cos' u v
  | u == (0, 0) || v == (0, 0) = error "Null vector given"
  | otherwise = (dot u v) / (norm u * norm v)

-- 1.f
areParallel u v = (fst u / fst v) == (snd u / snd v)

-- 2.
splitAt' n xs
  | length xs <= n = error "n is out of range"
  | n <= 0 = error "n is out of range"
  | otherwise = (take n xs, drop n xs)

-- 3.
double (i, x) = if even i then 2 * x else x
doubledList xs = [double (i, digitToInt x) | (i, x) <- zip [1..] (reverse xs)]
digitSum xs = sum [x `div` 10 + x `mod` 10 | x <- (doubledList xs)]
luhn xs = digitSum xs `mod` 10 == 0

-- 4.
factorize :: Int -> [Int]
factorize x = [y | y <- [1..x], x `mod` y == 0]

primes :: Int -> [Int]
primes n = take n [x | x <- [2..], length (factorize x) == 2]

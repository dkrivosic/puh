-- == Lecture 5 ================================================================
-- 1.1
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- 1.2
headsOf :: [[a]] -> [a]
headsOf []          = []
headsOf ((x:xs):ys) = [x] ++ headsOf ys

-- 2.1
modMult n m xs = modMult' (n `mod` m) xs
  where modMult' _ []     = []
        modMult' m (x:xs) = x * m : modMult' m xs

-- 2.2
addPredecessor (x:xs) = x : add x xs
  where add _ []      = []
        add p (x:xs)  = p + x : add x xs

-- 3.1
equalTriplets [] = []
equalTriplets (t@(a, b, c):xs)
  | a==b && b==c = t : equalTriplets xs
  | otherwise    = equalTriplets xs

-- 3.2
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x

-- 4.1
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0     = reverse $ drop' (-n) (reverse xs)
  | otherwise = drop' n xs

-- 4.2
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo 0 (-1) _     = []
takeFromTo 0 n2 (x:xs)  = x : takeFromTo 0 (n2-1) xs
takeFromTo n1 n2 (x:xs) = takeFromTo (n1-1) (n2-1) xs

-- 5.1
eachThird :: [a] -> [a]
eachThird (_:_:z:xs) = z : eachThird xs
eachThird _ = []

-- 5.2
crossZip (x1:x2:xs) (y1:y2:ys) = [(x1, y2), (x2, y1)] ++ crossZip xs ys
crossZip _ _ = []

-- 6.1
length'' xs = len 0 xs
  where len n []     = n
        len n (_:xs) = len (n+1) xs

-- 6.2
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip []     = error "empty list"
maxUnzip (h:xs) = maxUnzip' h xs
  where maxUnzip' (x, y) []          = (x, y)
        maxUnzip' (x, y) ((a, b):xs) = maxUnzip' (max x a, max y b) xs

maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' []          = error "empty list"
maxUnzip' [t]         = t
maxUnzip' ((x, y):xs) = (max x (fst $ maxUnzip' xs), max y (snd $ maxUnzip' xs))

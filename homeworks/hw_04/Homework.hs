import Data.List
import Data.Char

-- 1
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' xs (ys:yss) = ys ++ concatMap (xs ++) yss

-- 2.a
chunk :: Int -> [a] -> [[a]]
chunk n xs = chunk' n xs []
  where chunk' 0 _ _   = []
        chunk' _ [] ys = reverse ys
        chunk' n xs ys =  chunk' n (drop n xs) ((take n xs):ys)

-- 2.b
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy is xs = chunkBy' is xs []
  where chunkBy' [] _ ys      = reverse ys
        chunkBy' _ [] ys      = reverse ys
        chunkBy' (0:is) xs ys =  chunkBy' is xs ys
        chunkBy' (i:is) xs ys =  chunkBy' is (drop i xs) ((take i xs):ys)

-- 2.c
chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 _  = []
chunkInto n xs = chunkBy is xs
  where chunkSize = max 1 $ (length xs) `div` n
        lastChunk = chunkSize + (length xs) `mod` n
        is        = (replicate (n-1) chunkSize) ++ [lastChunk]

-- 3
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _  = []
cycleMap fs xs = map (\xf -> (snd xf) (fst xf)) $ zip xs $ concat $ repeat fs

-- 4.a
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ acc []     = acc
reduce f acc (x:xs) = reduce f (f acc x) xs

-- 4.b
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f []     = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

-- 4.c
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan f acc xs = scan' f [acc] xs
  where scan' _ ys [] = reverse ys
        scan' f ys (x:xs) = scan' f (result:ys) xs
          where result = f (head ys) x

-- 4.d
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f acc xs = rreduce' f acc (reverse xs)
  where rreduce' _ acc []     = acc
        rreduce' f acc (x:xs) = rreduce' f (f x acc) xs

-- 4.e
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 f [] = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)

-- 4.f
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan f acc xs = rscan' f [acc] xs
  where rscan' _ ys [] = ys
        rscan' f ys xs = rscan' f (result:ys) (init xs)
          where result = f (last xs) (head ys)

-- 5.a
type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton t n = newton' 1 t n where
  newton' x t n
    | x < 0            = error "can’t get sqrt of negative number"
    | abs(x^2 - n) < t = x
    | otherwise        = newton' (x - (x^2 - n) / (2 * x)) t n

-- 5.b
deriv :: (Double -> Double) -> Double -> Double
deriv f x = ((f (x + dx)) - (f x)) / dx
  where dx = 0.00001

-- 6
type Operators = [(Char, Int -> Int -> Int)]
basic    = [ ('+', (+))
             , ('-', (-)) ] :: Operators
standard = [ ('+', (+))
           , ('-', (-))
           , ('*', (*))
           , ('/', div)
           , ('^', (^)) ] :: Operators

rpnCalc :: String -> Operators -> Int
rpnCalc "" _ = 0
rpnCalc expr operators = rpnCalc' expr [] where
  rpnCalc' [] stack = head stack
  rpnCalc' expr@(x:xs) stack
    | invalidExpression = error "Invalid RPN expression"
    | isDigit x         = rpnCalc' xs ((digitToInt x) : stack)
    | invalidOperator   = error ("Invalid symbol " ++ (show x))
    | otherwise         = rpnCalc' xs (result : (drop 2 stack))
      where op                = head $ map snd $ filter (\o -> (fst o) == x) operators
            result            = op (head $ tail $ stack) (head $ stack)
            invalidExpression = (elem x (map fst operators)) && ((length stack) < 2)
            invalidOperator   = not (elem x (map fst operators))

-- 8
permutations' :: [a] -> [[a]]

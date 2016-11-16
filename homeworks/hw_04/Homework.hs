import Data.List

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

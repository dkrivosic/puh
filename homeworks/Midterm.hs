import Data.Char

-- 1
capitalise :: String -> [String] -> String
capitalise xs forbidden = capitalise' (words xs) forbidden []
  where capitalise' [] _ ys  = unwords $ reverse ys
        capitalise' (x:xs) forbidden ys
          | elem x forbidden = capitalise' xs forbidden (x : ys)
          | otherwise        = capitalise' xs forbidden (((toUpper $ head x) : (tail x)) : ys)

-- 2
removeNumbers :: [String] -> [String]
removeNumbers = map (filter (not . isDigit))

vowelCount :: String -> Int
vowelCount = length . filter (`elem` ['a', 'e', 'i', 'o', 'u']) . map toLower

twoInARow :: String -> Bool
twoInARow xs = (>0) $ length $ filter (\x -> (fst x) == (snd x)) $ zip xs (tail xs)

weirdFilter :: [String] -> [String]
weirdFilter = filter (not . twoInARow) . filter ((< 4) . vowelCount) . removeNumbers


-- 3
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

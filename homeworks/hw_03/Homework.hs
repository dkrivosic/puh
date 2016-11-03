import Data.List

type RomanNumeral = String
-- 1
containsValidLetters :: RomanNumeral -> Bool
containsValidLetters []     = True
containsValidLetters (x:xs) = (elem x ['I', 'V', 'X', 'L', 'C', 'D', 'M']) && containsValidLetters xs

correctRepetition :: RomanNumeral -> Bool
correctRepetition [] = True
correctRepetition s@(x:xs)
  | elem x ['V', 'L', 'D']      = repeatsLessThan 1 s && correctRepetition(xs)
  | elem x ['I', 'X', 'C', 'M'] = repeatsLessThan 3 s && correctRepetition(xs)
  where repeatsLessThan n s@(x:xs) = ((replicate (n+1) x) /= (take (n+1) s))

subtractiveCombination :: (Char, Char) -> Bool
subtractiveCombination (x, y) = elem [x, y] ["IV", "IX", "XL", "XC", "CD", "CM"]

additiveCombination :: (Char, Char) -> Bool
additiveCombination (x, y) =
  elem [x, y] [[snd x, snd y] | x <- zip [1..] digits, y <- zip [1..] digits, fst x >= fst y]
  where digits = ['I', 'V', 'X', 'L', 'C', 'D', 'M']

correctOrdering :: RomanNumeral -> Bool
correctOrdering []       = True
correctOrdering [x]      = True
correctOrdering (x:y:xs) =
  (additiveCombination (x, y) || subtractiveCombination (x, y)) && correctOrdering (y:xs)

getValue :: Char -> Int
getValue 'I' = 1
getValue 'V' = 5
getValue 'X' = 10
getValue 'L' = 50
getValue 'C' = 100
getValue 'D' = 500
getValue 'M' = 1000
getValue _ = error "unknown roman digit"

digitValues :: RomanNumeral -> [Int]
digitValues []                    = []
digitValues [x]                   = [getValue x]
digitValues (x:y:xs)
  | subtractiveCombination (x, y) = (getValue y - getValue x) : digitValues xs
  | otherwise                     = (getValue x) : digitValues (y:xs)

reducingValues :: RomanNumeral -> Bool
reducingValues xs = reducingValues' (digitValues xs)
  where reducingValues' []       = True
        reducingValues' [_]      = True
        reducingValues' (x:y:xs) = (x >= y) && reducingValues'(y:xs)

-- 1.a
isValidRoman :: RomanNumeral -> Bool
isValidRoman s = containsValidLetters s && correctRepetition s && correctOrdering s && reducingValues s

-- 1.b
toRoman :: Int -> RomanNumeral
toRoman n = toRoman' n [] [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"),
                        (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
  where toRoman' 0 s _ = s
        toRoman' n s (x:xs)
          | (n < 1) || (n > 3999) = error "Number cannot be represented"
          | repeats > 0   = toRoman' rest (s ++ numerals) (x:xs)
          | otherwise             = toRoman' n s xs
          where repeats  = n `div` (fst x)
                rest     = n `mod` (fst x)
                numerals = concat $ replicate repeats (snd x)
--1.c
fromRoman :: RomanNumeral -> Int
fromRoman xs
  | isValidRoman xs = sum $ digitValues xs
  | otherwise       = error "Not a valid Roman numeral"


-- 2
shortestDistance home pubs = shortestDistance' home home pubs
  where dist (x1, y1) (x2, y2)                    = abs(x1 - x2) + abs(y1 - y2)
        shortestDistance' home lastVisited []     = dist home lastVisited
        shortestDistance' home lastVisited pubs   =
          minimum [dist lastVisited pub + shortestDistance' home pub (delete pub pubs) | pub <- pubs]

-- 3
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]
x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- 3.a
mean :: DiscreteRandVar -> Double
mean []            = 0
mean ((x, p) : xs) = fromIntegral(x) * p + mean xs

mean' :: DiscreteRandVar -> Double
mean' xs = mean'' xs 0
  where mean'' [] m            = m
        mean'' ((x, p) : xs) m = mean'' xs (fromIntegral(x) * p + m)

-- 3.b
variance :: DiscreteRandVar -> Double
variance []          = 0
variance xs = variance'' xs (mean' xs)
  where variance'' [] _            = 0
        variance'' ((x, p) : xs) m = p * (fromIntegral(x) - m) ^ 2 + variance'' xs m

variance' :: DiscreteRandVar -> Double
variance' xs = variance'' xs (mean' xs) 0
  where variance'' [] _ v            = v
        variance'' ((x, p) : xs) m v = variance'' xs m (v + p * (fromIntegral(x) - m) ^ 2)

-- 3.c
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter p [] = []
probabilityFilter p ((x, px) : xs)
  | px >= p   = x : probabilityFilter p xs
  | otherwise = probabilityFilter p xs

probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' p xs = probabilityFilter'' p xs []
  where probabilityFilter'' _ [] fs = reverse fs
        probabilityFilter'' p ((x, px) : xs) fs
          | px >= p   = probabilityFilter'' p xs (x : fs)
          | otherwise = probabilityFilter'' p xs fs

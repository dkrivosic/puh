import Data.List

type RomanNumeral = String
-- 1.a
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

isValidRoman :: RomanNumeral -> Bool
isValidRoman s = containsValidLetters s && correctRepetition s && correctOrdering s && reducingValues s

-- 1.b
toRoman :: Int -> RomanNumeral

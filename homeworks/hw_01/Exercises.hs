import Data.Char
import Data.List

-- === Lecture 1 ===============================================================
-- 1.1.
concat3 x y z = x ++ (if length y < 2 then "" else y) ++ z

-- 1.2.
showSalary' amount bonus =
  "Salary is " ++ show amount ++ (if bonus /= 0 then ", and bonus " ++ show bonus else "")

-- === Lecture 2 ===============================================================
-- 1.1.
middle xs = reverse $ drop 3 $ reverse $ drop 3 xs

-- 1.2.
initials firstName lastName = [head firstName] ++ ". " ++ [head lastName] ++ "."

-- 1.3.
longestFirst s1 s2 | length s1 >= length s2 = s1 ++ s2
                   | otherwise = s2 ++ s1

-- 1.4.
safeHead l | null l = []
           | otherwise = [head l]

-- 1.5.
hasDuplicates xs = xs /= nub xs

-- 2.1.
doublesFromTo a b
  | b < a     = doublesFromTo b a
  | otherwise = [x*2 | x <- [a..b]]

-- 2.2
caesarCode n xs = [chr $ ((ord c - ord 'a' + n) `mod` (ord 'z' - ord 'a' + 1)) + ord 'a' | c <- xs]

-- 3.1.
lengths xss = [length xs | xs <- xss]
totalLength xss = sum $ lengths xss
letterCount s = totalLength [w | w <- words s, length w >= 3]

-- 3.2.
lowercaseNoSpaces xs = [toLower c | w <- words xs, c <- w]
isPalindrome xs = lowercaseNoSpaces xs == reverse(lowercaseNoSpaces xs)

-- 3.3.
flipp xss = concat $ reverse [reverse(xs) | xs <- xss]

-- 4.1.
inCircle r x y grid = [(a, b) | a <- fst grid, b <- snd grid, (x-a)^2 + (y-b)^2 <= r^2]

-- 4.2.
steps xs = [ (snd x, snd y) | x <- zip [1..] xs, y <- zip [1..] xs, fst y - fst x == 1]

-- 5.1
indices x xs = [ fst ix | ix <- zip [0..] xs, snd ix == x]

-- 5.2
showLineNumbers s = unlines [[chr $ i + ord '0'] ++ " " ++ x | (i, x) <- zip [1..] (lines s)]

-- 5.3.
haveAlignment xs ys = or [ix == iy | ix <- zip [1..] xs, iy <- zip [1..] ys]
common xs ys = [snd ix | ix <- zip [1..] xs, iy <- zip [1..] ys, ix == iy]

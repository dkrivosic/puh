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

-- === Lecture 3 ===============================================================
-- 1
foo10 w = [x ++ y | x <- lines w, y <- lines w] -- String -> [String]
foo11 w = [(x,y) | x <- lines w, y <- lines w] -- String -> [(String, String)]
foo12 w = [y : x | x <- lines w, y <- w] -- String -> [String]
foo13 w = [(y:x, w) | x <- lines w, y <- w] -- String -> [(String, String)]
foo14 w = [(x, x=='a') | x <- w ] -- String -> [(Char, Bool)]
foo15 s = tail [ c | c <- s, isLower c ] -- String -> String
foo16 s = zip [ c | c <- s, isLower c ] "Haskell" -- String -> [(Char, Char)]
foo17 n c = reverse $ drop n $ c : "Haskell" -- Int -> Char -> String
foo18 xs = last $ words xs -- String -> String
foo19 x z = x : 'y' : z -- Char -> String -> String

-- 2
foo20 xs = tail xs ++ [head xs] -- [a] -> [a]
foo21 xs = (head xs, tail xs) -- [a] -> (a, [a])
foo22 x xs = x:xs -- a -> [a] -> [a]
foo23 l = init $ tail l -- [a] -> [a]
foo24 xss ys = concat xss ++ ys -- [[a]] -> [a] -> [a]
foo25 xss ys = (head $ concat xss, head ys) -- [[a]] -> [b] -> (a, b)
foo26 xs = head $ concat $ concat xs -- [[[a]]] -> a
foo27 cs = [[c1,c2] | c1 <- cs, c2 <- cs] -- [a] -> [[a]]
foo28 cs = [concat [c1,c2] | c1 <- cs, c2 <- cs] -- [[a]] -> [[a]]
foo29 cs = concat [[c1,c2] | c1 <- cs, c2 <- cs] -- [a] -> [a]

-- 3
foo30 x ys = if x==head ys then x else last ys -- Eq a => a -> [a] -> a
foo31 x ys = if x < head ys then x else last ys -- Ord a => a -> [a] -> a
foo32 xs yss = if xs==head yss then head xs else last xs -- Eq a => [a] -> [[a]] -> a
foo33 x ys = if x then zip [1..9] ys else [] -- (Num a, Enum a) => Bool -> [b] -> [(a, b)]
foo34 w = zip [0..] (lines w) -- (Num a, Enum a) => String -> [(a, String)]
foo35 x y = if odd x then y else x / 10 -- (Integral t, Fractional t) => t -> t -> t
foo36 xs = sort xs == xs -- Ord a => [a] -> Bool
foo37 x xs = show x ++ (show $ concat xs) -- (Foldable t, Show a1, Show a) => a -> t [a1] -> [Char]
foo38 xs = sum $ concat xs -- Num a => [[a]] -> a
foo39 xs yss = sum $ [min x y | x <- xs, ys <- yss, y <- ys] -- (Ord a, Num a) => [a] -> [[a]] -> a

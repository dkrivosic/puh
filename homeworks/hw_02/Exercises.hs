import Data.Char
import Data.List

-- === Lecture 4 ===============================================================
-- 1.1
headHunter :: [[a]] -> a
headHunter ((x:_):_)     = x
headHunter (_:(x:_):_)   = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "None of the above worked"

-- 1.2
firstColumn m = [h | (h:_) <- m ]

-- 1.3
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [ a:a:a:xs | (a:xs) <- words s]

-- 2.1
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) =
  (toUpper x : (adjust len xs), toUpper y : (adjust len ys))
  where len        = max(length xs) (length ys)
        adjust n s = s ++ replicate (n - length s) ' '

-- 2.2
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

quartiles :: [Int] -> (Double,Double,Double)
quartiles xs = (first_q, second_q, third_q)
  where ls       = sort xs
        second_q = median ls
        mid      = length ls `div` 2
        halves   = if odd $ length ls then (take mid ls, drop (mid+1) ls) else splitAt mid ls
        first_q  = median $ fst halves
        third_q  = median $ snd halves

-- 3.1
pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = let
      len        = max(length xs) (length ys)
      adjust n s = s ++ replicate (n - length s) ' '
  in (toUpper x : (adjust len xs), toUpper y : (adjust len ys))

quartiles' :: [Int] -> (Double,Double,Double)
quartiles' xs = let
    ls       = sort xs
    second_q = median ls
    mid      = length ls `div` 2
    halves   = if odd $ length ls then (take mid ls, drop (mid+1) ls) else splitAt mid ls
    first_q  = median $ fst halves
    third_q  = median $ snd halves
  in (first_q, second_q, third_q)

-- 4.1
describePair (a,b) c =
  "The pair " ++ case (a, b) of
    (1, 1) -> "contains two ones"
    (1, _) -> "contains one one"
    (_, 1) -> "contains one one"
    _ -> "does not contain a single one"
  ++ " and the second element of the list is " ++ case c of
    (a:b:_) -> show b
    _ -> "unavailable"

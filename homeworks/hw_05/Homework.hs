import Data.List
import Data.Char

-- 1
-- x and y coordinates
type Position = (Integer, Integer)
data Orientation = West | East | North | South deriving (Eq, Show)
-- Clockwise and Counterclockwise
data TurnDir = CW | CCW deriving (Eq, Show)

-- 1.a,b,c
data Turtle = Turtle
  { position :: Position
  , orientation :: Orientation } deriving Show

-- 1.d
newTurtle :: Turtle
newTurtle = Turtle (0, 0) North

-- 1.e
move :: Integer -> Turtle -> Turtle
move step (Turtle (x, y) o)
  | step < 0  = error "Turtles cannot move backwards"
  | otherwise = case o of
    West -> Turtle (x - step, y) o
    East -> Turtle (x + step, y) o
    North -> Turtle (x, y + step) o
    South -> Turtle (x, y - step) o

-- 1.f
turn :: TurnDir -> Turtle ->  Turtle
turn dir (Turtle pos o)
  | dir == CW = case o of
    North -> Turtle pos East
    East  -> Turtle pos South
    South -> Turtle pos West
    West  -> Turtle pos North
  | otherwise = case o of
    North -> Turtle pos West
    West  -> Turtle pos South
    South -> Turtle pos East
    East  -> Turtle pos North

-- 1.g
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle moves turtle = foldr (\f x -> f x) turtle (reverse moves)

-- 4.a
sortTracks :: [String] -> [String]
sortTracks = sortBy (\x y -> (findNumber x) `compare` (findNumber y))
  where findNumber = head . filter (isDigit . head) . words

-- 4.b
numberOfPlays :: [String] -> Integer
numberOfPlays = sum . map (parseInt . head . words)
  where parseInt x = read x :: Integer

-- 5
substrings :: Int -> String -> [String]
substrings n s = substrings' (length s) n s
    where substrings' l n s
              | l >= n    = take n s : substrings' (l - 1) n (tail s)
              | otherwise = []

possibleRoutes :: String -> Int
possibleRoutes xs = 2^ambiguous
  where ambiguous = length $ filter (`elem` ["NE", "NW", "SE", "SW"]) $ substrings 2 xs

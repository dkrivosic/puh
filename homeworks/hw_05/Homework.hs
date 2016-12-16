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

-- 2
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- 2.a
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter f Leaf = Leaf
treeFilter f (Node x lt rt)
  | f x       = Node x (treeFilter f lt) (treeFilter f rt)
  | otherwise = Leaf

-- 2.b
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f n = levelMap' f 0 n
  where levelMap' _ _ Leaf             = Leaf
        levelMap' f lvl (Node x lt rt) =
           Node (f lvl x) (levelMap' f (lvl + 1) lt) (levelMap' f (lvl + 1) rt)

-- 2.c
equalTrees :: Eq a => Tree a -> Tree a -> Bool
equalTrees Leaf Leaf = True
equalTrees _ Leaf = False
equalTrees Leaf _ = False
equalTrees (Node x1 lt1 rt1) (Node x2 lt2 rt2)
  | x1 /= x2  = False
  | otherwise = (equalTrees lt1 lt2) && (equalTrees rt1 rt2)

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf Leaf = True
isSubtree _ Leaf = False
isSubtree t1 t2@(Node _ lt rt) = (equalTrees t1 t2) || (isSubtree t1 lt) || (isSubtree t1 rt)

-- 3
data Pred = And Pred Pred | Or Pred Pred | Not Pred | Val Bool deriving Show

expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))

eval :: Pred -> Bool
eval (Val x)   = x
eval (Not x)   = not (eval x)
eval (And x y) = (eval x) && (eval y)
eval (Or x y)  = (eval x) || (eval y)

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

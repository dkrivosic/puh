import Control.Exception
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.Random
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set

-- == Lecture 10 ================================================================
data Sex = Male | Female deriving (Show,Read,Eq,Ord)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

-- 1.1.
foo = foo

-- 1.2.
fathersChildren :: Person2 -> [Person2]
fathersChildren = maybe [] (children2) . father2

mothersChildren :: Person2 -> [Person2]
mothersChildren = maybe [] (children2) . mother2

parentCheck :: Person2 -> Bool
parentCheck p = p `elem` fathersChildren p ++ mothersChildren p

-- 1.3.
sister :: Person2 -> Maybe Person2
sister p = sister' (filter ((/=(personId2 p)) . personId2) $ filter ((Female==) . sex2) $ mothersChildren p)
  where sister' []    = Nothing
        sister' (x:_) = Just x

-- 1.4.
descendant :: Person2 -> [Person2]
descendant p = children ++ concatMap descendant children
  where children = children2 p

-- 2.1
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

listHead :: MyList a -> Maybe a
listHead Empty      = Nothing
listHead (Cons x _) = Just x

-- 2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty       = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

-- 3.1
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

treeMax :: Ord a => Tree a -> a
treeMax (Node x Null Null) = x
treeMax Null               = error "Empty Tree"
treeMax (Node x Null rb)   = treeMax rb
treeMax (Node x lb Null)   = treeMax lb
treeMax (Node x lb rb)     = x `max` treeMax lb `max` treeMax rb

-- 3.2
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node x lt rt) = (treeToList lt) ++ [x] ++ (treeToList rt)

-- 3.3
levelCut :: Int -> Tree a -> Tree a
levelCut lvl Null           = Null
levelCut 0 (Node y l r)     = Node y Null Null
levelCut lvl (Node x lt rt) = Node x (levelCut (lvl-1) lt) (levelCut (lvl-1) rt)

-- 4.1
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree = (foldr treeInsert Null) . reverse

-- 4.2
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree


-- 5.1
data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = False
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- 5.2
data Person3 = Person3 {
  personId3 :: String,
  forename3 :: String,
  surname3  :: String,
  sex3      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother3   :: Maybe Person3,
  father3   :: Maybe Person3,
  partner3  :: Maybe Person3,
  children3 :: [Person3] } deriving (Read,Eq,Ord)

instance Show Person3 where
  show p = forename3 p ++ " " ++ surname3 p

-- 6.1
instance (Eq a) => Eq (MyList a) where
  (Cons x _) == (Cons y _) = (x == y)
  Empty == Empty           = True
  _ == _                   = False

-- == Lecture 11 ================================================================
-- 1.1
main1 = do
  first <- getLine
  second <- getLine
  putStrLn $ second ++ first

-- 1.2
threeNumbers = do
  x <- getLine
  y <- getLine
  z <- getLine
  print $ read x + read y + read z

-- 2.1
threeStrings :: IO Int
threeStrings = do
  x <- getLine
  y <- getLine
  z <- getLine
  let w = x ++ y ++ z
  putStrLn $ w
  return . length $ w

-- 2.2
askNumber9 :: IO Int
askNumber9 = do
  x <- getLine
  if all isDigit x then return $ read x else askNumber9

-- 2.4
inputStrings :: IO [String]
inputStrings = do
  x <- getLine
  if x == "" then return [] else do
    y <- inputStrings
    return $ [x] ++ y

-- 3.1
readPrint :: IO ()
readPrint = do
  nn <- getLine
  let n = read nn
  let r = replicate n getLine
  xs <- sequence r
  mapM_ putStrLn $ reverse xs

-- 3.2
sequence' :: Monad m => [m a] -> m [a]
sequence' xs = sequence'' xs []
 where sequence'' [] acc       = return acc
       sequence'' (x : xs) acc = do
         y <- x
         sequence'' xs (y : acc)

sequence_' :: Monad m => [m a] -> m ()
sequence_' xs = sequence_'' xs []
  where sequence_'' [] acc       = return ()
        sequence_'' (x : xs) acc = do
          y <- x
          sequence_'' xs (y : acc)

-- 3.3
mapM'  :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f xs = mapM'' f xs []
  where mapM'' f [] acc       = return acc
        mapM'' f (x : xs) acc = do
          y <- f x
          mapM'' f xs (y : acc)

mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' f xs = mapM_'' f xs []
  where mapM_'' f [] acc       = return ()
        mapM_'' f (x : xs) acc = do
          y <- f x
          mapM_'' f xs (y : acc)

-- 3.4
pythagoreanTriplets :: IO ()
pythagoreanTriplets = mapM_ print $ filter (\t -> third t < 100) $
                      map (\t -> (fst t, snd t, sqrt $ (fst t)^2 + (snd t)^2)) $
                      concatMap (\x -> zip (repeat x) [x..100]) [1..100]
  where third (_, _, c) = c


-- 4.1
filterOdd :: IO ()
filterOdd = interact (unlines . map snd . filter ((==0) . (`mod` 2) . fst) . zip [1..] . lines)

-- 4.2
numberLines :: IO ()
numberLines = interact (unlines . map (\t -> (show $ fst t) ++ (snd t)) . zip [1..] . lines)

-- 4.3
filterWords :: [String] -> IO ()
filterWords s = interact (unwords . filter (`notElem` ["the", "of", "bla"]) . words)


-- 5.1
wc :: FilePath -> IO (Int, Int, Int)
wc path = withFile path ReadMode $ \h -> do
  s <- hGetContents h
  let c = length s
  let w = length $ words s
  let l = length $ lines s
  putStrLn $ show (c, w, l)
  return (c, w, l)

-- 5.2
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs path_r path_w = do
  h_r <- openFile path_r ReadMode
  h_w <- openFile path_w WriteMode
  s <- hGetContents h_r
  mapM_ (hPutStrLn h_w) (map snd . filter ((`elem` xs) . fst) . zip [1..] $ lines s)
  hClose h_r
  hClose h_w

-- 6.1.
wordTypes :: FilePath -> IO Int
wordTypes path = do
  s <- readFile path
  return . length . nub . words $ s

-- 6.2.
diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- readFile f1
  s2 <- readFile f2
  let s = zip (lines s1) (lines s2)
  mapM_ putStrLn $ concatMap (\t -> [('<' : (fst t)), ('>' : (snd t))]) $ filter (uncurry (/=)) s

-- 6.3.
removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  (ft, ht) <- openTempFile "" f
  s <- readFile f
  hPutStrLn ht $ unlines $ map (reverse . dropWhile (==' ') . reverse) $ lines s
  hClose ht
  renameFile ft f

-- 7.1
fileHead :: IO ()
fileHead = catch (fileHead') $ \e ->
  if isDoesNotExistError e then putStrLn "Error: File does not exist"
  else ioError e

fileHead' :: IO ()
fileHead' = do
  args <- getArgs
  let n = if null args then 10 else read $ head args
  f <- if (length args) < 2 then getLine else return $ last args
  s <- readFile f
  print . unlines . take n . lines $ s

-- 7.2
sortFiles :: IO ()
sortFiles = catch (sortFiles') $ \e ->
  if isDoesNotExistError e then putStrLn "Error: Some files do not exist"
  else ioError e

sortFiles' :: IO ()
sortFiles' = do
  args <- getArgs
  l <- mapM readFile args
  print . unlines . sort . concat . map lines $ l

-- 8.1
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = do
  let (r, gn) = random g
  r : randoms' gn

-- 8.2
randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions lx hx ly hy = return $ randomPositions' lx hx ly hy (mkStdGen 13)
  where randomPositions' lx hx ly hy g = do
          let (x, g1) = randomR (lx, hx) g
          let (y, g2) = randomR (ly, hy) g1
          (x, y) : randomPositions' lx hx ly hy g2

-- == Lecture 12 ================================================================
-- 1.1
data Person = Person {
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  mother   :: Maybe Person,
  father   :: Maybe Person,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)

grandfathersPartnerForename :: Person -> Maybe String
grandfathersPartnerForename p = mother p >>= father >>= partner >>= return . forename

-- 1.2
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s str = stripPrefix (reverse s) (reverse str) >>= return . reverse

removeAffixes :: String -> String -> String -> Maybe String
removeAffixes p s str = stripPrefix p str >>= stripSuffix s

-- 2.1
grandfathersPartnerForename' :: Person -> Maybe String
grandfathersPartnerForename' p = do
  f <- mother p
  g <- father f
  par <- partner g
  return $ forename par

-- 2.2
main5 :: IO ()
main5 = getArgs >>= (\x -> case x of
          (f:_) -> doesFileExist f >>= (\b -> if b then openFile f ReadMode else return stdin)
          []    -> return stdin) >>=
        hGetContents >>= putStr . unlines . sort . lines

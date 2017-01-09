-- 2
import CSVUtils
import Data.List
type Decision = String

type CSV = [Entry]
type Entry = [Field]
type Field = String

caseA = ["Raining", "Yes", "Workday"]
caseB = ["Sunny", "No", "Workday"]

my_csv = do
  readCSV ";" "./data.csv" >>= return . drop 1

nbDecide :: CSV -> [String] -> Decision
nbDecide csv x = do
  let d = (length $ head csv) - 1
  let decs = nub $ colFields d csv
  let xByDec = map (\x -> filter ((x==) . (!!d)) csv) decs
  let decsCnt = map length xByDec
  let pc = map ((/ fromIntegral(length csv)) . fromIntegral) decsCnt
  let xCsvZip = map (map (uncurry zip) . zip (repeat x) . map init) xByDec
  let eqByCol = map (map (map (uncurry (==)))) xCsvZip
  let eqByColInt = map (map (map (\b -> if b then 1 else 0))) eqByCol
  let decCnt = length decs
  let eqByFeature = map (\j -> map (\i -> map (!!i) (eqByColInt !! j)) [0..(d-1)]) [0..(decCnt-1)]
  let eqCnts = map (map sum) eqByFeature
  let pxc = map (\i -> map (/fromIntegral(decsCnt !! i)) (eqCnts !! i)) [0..(decCnt-1)]
  let probs = map (uncurry (*)) . zip pc . map product $ pxc
  let maxi = snd . maximum $ zip probs [0 .. ]
  decs !! maxi

nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll csv = map (nbDecide csv)

-- 3
class Truthy a where
  truey :: a -> Bool
  falsey :: a -> Bool
  truey  = not . falsey
  falsey = not . truey

instance Truthy Bool where
  truey True  = True
  truey False = False

instance Truthy Int where
  truey 0 = False
  truey _ = True

instance Truthy [a] where
  truey [] = False
  truey _  = True

-- 3.a
if' :: Truthy p => p -> a -> a -> a
if' p t f = if truey p then t else f

-- 3.b
assert :: Truthy p => p -> a -> a
assert p x = if truey p then x else error "Assertion failed"

-- 3.c
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) x y = truey x && truey y

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) x y = truey x || truey y

-- 4.a
data DiffList a = DiffList { undiff :: [a] -> [a] }

-- 4.b
empty :: DiffList a
empty = DiffList ([]++)

-- 4.c
fromList :: [a] -> DiffList a
fromList xs = DiffList (xs++)

-- 4.d
toList :: DiffList a -> [a]
toList ds = undiff ds []

-- 4.e
append :: DiffList a -> DiffList a -> DiffList a
append (DiffList f) (DiffList g) = DiffList (f . g)

-- 4.f
instance Monoid (DiffList a) where
  mempty = DiffList ([]++)
  mappend (DiffList f) (DiffList g) = DiffList (f . g)

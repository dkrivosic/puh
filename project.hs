import Data.Char

type Op = (String, Double -> Double -> Double)
data Node = Operator Op Node Node | Function String Node | Variable String | Constant Double

deriveBy :: String -> Node -> Node
deriveBy var x = simplify $ deriveBy' var x

deriveBy' :: String -> Node -> Node
deriveBy' _ (Constant _) = Constant 0
deriveBy' var (Variable x)
  | x == var = Constant 1
  | otherwise = Constant 0
deriveBy' var (Function f x)
  | f == "sin" = Operator ("*", (*)) (Function "cos" x) (deriveBy' var x)
  | f == "cos" = Operator ("*", (*)) (Operator ("*", (*)) (Constant (-1)) (Function "cos" x)) (deriveBy' var x)
  | f == "exp" = Operator ("*", (*)) (Function f x) (deriveBy' var x)
  | f == "log" = Operator ("*", (*)) (Operator ("/", (/)) (Constant 1) x) (deriveBy' var x)
  | otherwise = error "Unsupported function"
deriveBy' var (Operator ("*", _) x y) =
  Operator ("+", (+)) (Operator ("*", (*)) (deriveBy' var x) (y)) (Operator ("*", (*)) (x) (deriveBy' var y))
deriveBy' var (Operator ("/", _) x y) =
  Operator ("/", (/)) (Operator ("-", (-)) (Operator ("*", (*)) (deriveBy' var x) y) (Operator ("*", (*)) x (deriveBy' var y))) (Operator ("^", (**)) y (Constant 2))
deriveBy' var (Operator ("+", _) x y) = Operator ("+", (+)) (deriveBy' var x) (deriveBy' var y)
deriveBy' var (Operator ("-", _) x y) = Operator ("-", (-)) (deriveBy' var x) (deriveBy' var y)
deriveBy' var (Operator ("^", _) x (Constant y)) =
  Operator ("*", (*)) (Operator ("*", (*)) (Constant y) (Operator ("^", (**)) (x) (Constant (y-1)))) (deriveBy' var x)

evaluate :: Node -> Double
evaluate (Variable _)      = error "The tree still contains variables."
evaluate (Constant x)      = x
evaluate (Operator op x y) = (snd op) (evaluate x) (evaluate y)
evaluate (Function fun x)  = f (evaluate x)
  where f = snd $ head $ filter ((==fun) . fst) functions


substitute :: String -> Double -> Node -> Node
substitute var val x = simplify $ substitute' var val x

substitute' :: String -> Double -> Node -> Node
substitute' _ _ (Constant x)          = Constant x
substitute' var val (Function f x)    = Function f (substitute' var val x)
substitute' var val (Operator op x y) = Operator op (substitute' var val x) (substitute' var val y)
substitute' var val (Variable x)
  | var == x                         = Constant val
  | otherwise                        = Variable x

createExpression :: String -> Node
createExpression = simplify . buildExpressionTree . parseExpression

buildExpressionTree :: String -> Node
buildExpressionTree xs = buildTree (words xs) []
  where buildTree [] stack = head $ stack
        buildTree (x : xs) stack
          | isNumeric x = buildTree xs (Constant (read x) : stack)
          | isVariable x = buildTree xs (Variable x : stack)
          | isFunction x = buildTree xs ((Function x (head stack)) : (tail stack))
          | isOperator x = buildTree xs (Operator op left right : (drop 2 stack))
          | otherwise = error "Invalid char"
            where op    = head $ filter ((==x) . fst) operators
                  left  = head $ tail stack
                  right = head stack

parseExpression :: String -> String
parseExpression s = parseExpression' (words s) [] [] where
  parseExpression' [] [] q                = unwords $ reverse q
  parseExpression' [] (s : ss) q          = parseExpression' [] ss (s : q)
  parseExpression' (x : xs) s q
    | isNumeric x || isVariable x         = parseExpression' xs s (x : q)
    | isFunction x                        = parseExpression' xs (x : s) q
    | isOperator x && (not $ null s) && (isOperator $ head s) && (associativity x == "Left") &&
    (precedence x <= precedence (head s)) = parseExpression' (x : xs) (tail s) (head s : q)
    | isOperator x && (not $ null s) && (isOperator $ head s) && (associativity x == "Right") &&
    (precedence x < precedence (head s))  = parseExpression' (x : xs) (tail s) (head s : q)
    | isOperator x                        = parseExpression' xs (x : s) q
    | x == "("                            = parseExpression' xs (x : s) q
    | x == ")"                            = do
      let t = takeWhile (/="(") s
      let ss = tail $ dropWhile (/="(") s
      let (stack, queue) = if isFunction $ head ss then (tail ss, (head ss : ((reverse t) ++ q)))
                                                   else (ss, (reverse t) ++ q)
      parseExpression' xs stack queue
    | otherwise                           = error "Parse error: Invalid expression"

isVariable :: String -> Bool
isVariable x = not . or . map (\f -> f x) $ [isNumeric, isFunction, isOperator, isParenthesis]

isNumeric :: String -> Bool
isNumeric ""  = False
isNumeric "." = False
isNumeric xs  =
  case dropWhile isDigit xs of
    ""         -> True
    ('.' : ys) -> all isDigit ys
    _           -> False

functions = [("sin", sin), ("cos", cos), ("exp", exp), ("log", log)]

isFunction :: String -> Bool
isFunction = (`elem` (map fst functions))

operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/)), ("^", (**))] :: [Op]

isOperator :: String -> Bool
isOperator = (`elem` (map fst operators))

isParenthesis :: String -> Bool
isParenthesis = (`elem` ["(", ")"])

precedence :: String -> Int
precedence op
  | op `elem` ["^"]      = 4
  | op `elem` ["*", "/"] = 3
  | op `elem` ["+", "-"] = 2
  | otherwise            = 0

associativity :: String -> String
associativity op = case op of
  "^" -> "Right"
  _   -> "Left"

simplify :: Node -> Node
simplify t = if t == tt then t else simplify tt
  where tt = simplify' t

simplify' :: Node -> Node
simplify' (Constant x)                          = Constant x
simplify' (Variable x)                          = Variable x
simplify' (Function f x)                        = Function f (simplify' x)
simplify' (Operator ("*", (*)) (Constant 0) x)  = Constant 0
simplify' (Operator ("*", (*)) x (Constant 0))  = Constant 0
simplify' (Operator ("*", (*)) x (Constant 1))  = simplify' x
simplify' (Operator ("*", (*)) (Constant 1) x)  = simplify' x
simplify' (Operator ("*", (*)) x y)             = Operator ("*", (*)) (simplify' x) (simplify' y)
simplify' (Operator ("^", (**)) x (Constant 1)) = simplify' x
simplify' (Operator ("^", (**)) x (Constant 0)) = Constant 1
simplify' (Operator ("^", (**)) (Constant 1) x) = Constant 1
simplify' (Operator ("^", (**)) x y)            = Operator ("^", (**)) (simplify' x) (simplify' y)
simplify' (Operator ("/", (/)) (Constant 0) y)  = Constant 0
simplify' (Operator ("/", (/)) x (Constant 1))  = simplify' x
simplify' (Operator ("/", (/)) x y)             = Operator ("/", (/)) (simplify' x) (simplify' y)
simplify' (Operator ("+", (+)) (Constant 0) y)  = simplify' y
simplify' (Operator ("+", (+)) x (Constant 0))  = simplify' x
simplify' (Operator ("+", (+)) x y)             = Operator ("+", (+)) (simplify' x) (simplify' y)
simplify' (Operator ("-", (-)) (Constant 0) y)  = Operator ("*", (*)) (Constant (-1)) (simplify' y)
simplify' (Operator ("-", (-)) x (Constant 0))  = simplify' x
simplify' (Operator ("-", (-)) x y)             = Operator ("-", (-)) (simplify' x) (simplify' y)

instance Eq Node where
  (Constant x) == (Constant y)                     = x == y
  (Variable x) == (Variable y)                     = x == y
  (Function f x) == (Function g y)                 = f == g && x == y
  (Operator (o1, _) a b) == (Operator (o2, _) c d) = o1 == o2 && a == c && b == d
  _ == _                                           = False

instance Show Node where
  show (Constant x)                                            = show x
  show (Variable x)                                            = x
  show (Function f x)                                          = f ++ " ( " ++ show x ++ " ) "
  show (Operator ("*", _) x@(Operator (op1, _) _ _) y@(Operator (op2, _) _ _))
    | (op1 == "+" || op1 == "-") && (op2 == "+" || op2 == "-") = " ( " ++ show x ++ " ) * ( " ++ show y ++ " ) "
    | op1 == "+" || op1 == "-"                                 = " ( " ++ show x ++ " ) * " ++ show y
    | op2 == "+" || op2 == "-"                                 = show x ++ " * ( " ++ show y ++ " ) "
    | otherwise                                                = show x ++ " * " ++ show y
  show (Operator ("*", _) x y@(Operator (op, _) _ _))
    | op == "+" || op == "-"                                   = show x ++ " * ( " ++ show y ++ " ) "
    | otherwise                                                = show x ++ " * " ++ show y
  show (Operator ("*", _) x@(Operator (op, _) _ _) y)
    | op == "+" || op == "-"                                   = "( " ++ show x ++ " ) * " ++ show y
    | otherwise                                                = show x ++ " * " ++ show y
  show (Operator ("/", _) x@(Operator (op1, _) _ _) y@(Operator (op2, _) _ _))
    | (op1 == "+" || op1 == "-") && (op2 == "+" || op2 == "-") = " ( " ++ show x ++ " ) / ( " ++ show y ++ " ) "
    | op1 == "+" || op1 == "-"                                 = " ( " ++ show x ++ " ) / " ++ show y
    | op2 == "+" || op2 == "-"                                 = show x ++ " / ( " ++ show y ++ " ) "
    | otherwise                                                = show x ++ " / " ++ show y
  show (Operator ("/", _) x y@(Operator (op, _) _ _))
    | op == "+" || op == "-"                                   = show x ++ " / ( " ++ show y ++ " ) "
    | otherwise                                                = show x ++ " / " ++ show y
  show (Operator ("/", _) x@(Operator (op, _) _ _) y)
    | op == "+" || op == "-"                                   = "( " ++ show x ++ " ) / " ++ show y
    | otherwise                                                = show x ++ " / " ++ show y
  show (Operator ("^", _) x@(Operator (op, _) _ _) y)
    | op == "+" || op == "-"                                   = " ( " ++ show x ++ " ) ^ " ++ show y
    | otherwise                                                = show x ++ " " ++ op ++ " " ++ show y
  show (Operator op x y)                                       = show x ++ " " ++ fst op ++ " " ++ show y

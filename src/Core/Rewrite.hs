module Core.Rewrite (
    eval,
    simplify
) where
import Core.Types ( Expr(..) )
import Data.Bifunctor (Bifunctor)
import Data.Set

eval :: Expr -> Expr
eval e = case e of
  Number x -> Number x
  Symbol x -> Symbol x
  Add a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (x + y)
    (x, y) -> Add x y
  Sub a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (x - y)
    (x, y) -> Sub x y
  Mul a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (x * y)
    (x, y) -> Mul x y
  Div a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (x / y)
    (x, y) -> Div x y
  Neg a -> case eval a of
    Number x -> Number (-x)
    x -> Neg x
  Abs a -> case eval a of
    Number x -> Number (abs x)
    x -> Abs x
  Pow a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (x ** y)
    (x, y) -> Pow x y
  Log a b -> case (eval a, eval b) of
    (Number x, Number y) -> Number (logBase x y)
    (x, y) -> Log x y
  Ln a -> case eval a of
    Number x -> Number (log x)
    x -> Ln x
  Sin a -> case eval a of
    Number x -> Number (sin x)
    x -> Sin x
  Cos a -> case eval a of
    Number x -> Number (cos x)
    x -> Cos x
  Tan a -> case eval a of
    Number x -> Number (tan x)
    x -> Tan x
  Sqrt a -> case eval a of
    Number x -> Number (sqrt x)
    x -> Sqrt x
  Asin a -> case eval a of
    Number x -> Number (asin x)
    x -> Asin x
  Acos a -> case eval a of
    Number x -> Number (acos x)
    x -> Acos x
  Atan a -> case eval a of
    Number x -> Number (atan x)
    x -> Atan x
  Sinh a -> case eval a of
    Number x -> Number (sinh x)
    x -> Sinh x
  Cosh a -> case eval a of
    Number x -> Number (cosh x)
    x -> Cosh x
  Tanh a -> case eval a of
    Number x -> Number (tanh x)
    x -> Tanh x
  Asinh a -> case eval a of
    Number x -> Number (asinh x)
    x -> Asinh x
  Acosh a -> case eval a of
    Number x -> Number (acosh x)
    x -> Acosh x
  Atanh a -> case eval a of
    Number x -> Number (atanh x)
    x -> Atanh x
  Pi -> Number pi
  E -> Number (exp 1)

simplify :: Expr -> Expr
simplify (Number x) = Number x
simplify (Symbol x) = Symbol x
simplify (Add a b) = case (simplify a, simplify b) of
    (Number 0, x) -> x
    (x, Number 0) -> x
    (x, y) | x == y -> Number 0
    (Number x, Number y) -> Number (x + y)
    (Log x a', Log y b') | x == y -> Log x $ a' * b'
    (x, y) -> Add x y
simplify (Sub a b) = case (simplify a, simplify b) of
    (x, Number 0) -> x
    (x, y) | x == y -> Number 0
    (Number x, Number y) -> Number (x - y)
    (x, y) -> Sub x y
simplify (Mul a b) = case (simplify a, simplify b) of
    (Number 0, _) -> Number 0
    (_, Number 0) -> Number 0
    (Number 1, x) -> x
    (x, Number 1) -> x
    (Pow x a', Pow y b') | x == y -> Pow x $ a' + b'
    (x, y) -> Mul x y
simplify (Div a b) = case (simplify a, simplify b) of
    (_, Number 0) -> error "Division by zero"
    (x, Number 1) -> x
    (x, y) | x == y -> Number 1
    (Number 0, _) -> Number 0
    (x, y) -> Div x y
simplify (Neg a) = case simplify a of
    Number x -> Number (-x)
    Neg (Neg x) -> x
    x -> Neg x
simplify (Abs a) = case simplify a of
    Number x -> Number (abs x)
    Abs (Abs x) -> Abs x
    x -> Abs x
simplify (Pow a b) = case (simplify a, simplify b) of
    (Number 0, Number 0) -> error "0^0 is undefined"
    (_, Number 0) -> Number 1
    (x, Number 1) -> x
    (Number 0, _) -> Number 0
    (Number 1, _) -> Number 1
    (Pow x y, z) -> Pow x $ y * z
    (x, y) -> Pow x y
simplify (Log a b) = case (simplify a, simplify b) of
    (_, Number 0) -> error "log 0 is undefined"
    (_, Number 1) -> Number 0
    (E, x) -> Ln x
    (x, y) -> Log x y
simplify (Ln a) = case simplify a of
    Number x -> Number (log x)
    x -> Ln x
simplify (Sin a) = case simplify a of
    Sin (Asin x) -> x
    x -> Sin x
simplify (Cos a) = case simplify a of
    Cos (Acos x) -> x
    x -> Cos x
simplify (Tan a) = case simplify a of
    Tan (Atan x) -> x
    x -> Tan x
simplify (Sqrt a) = case simplify a of
    Number x | isPerfectSquare x -> Number (sqrt x)
    Sqrt (Pow x (Number 2)) -> x
    x -> Sqrt x
simplify (Asin a) = case simplify a of
    Asin (Sin x) -> x
    x -> Asin x
simplify (Acos a) = case simplify a of
    Acos (Cos x) -> x
    x -> Acos x
simplify (Atan a) = case simplify a of
    Atan (Tan x) -> x
    x -> Atan x
simplify (Sinh a) = case simplify a of
    Sinh (Asinh x) -> x
    x -> Sinh x
simplify (Cosh a) = case simplify a of
    Cosh (Acosh x) -> x
    x -> Cosh x
simplify (Tanh a) = case simplify a of
    Tanh (Atanh x) -> x
    x -> Tanh x
simplify (Asinh a) = case simplify a of
    Asinh (Sinh x) -> x
    x -> Asinh x
simplify (Acosh a) = case simplify a of
    Acosh (Cosh x) -> x
    x -> Acosh x
simplify (Atanh a) = case simplify a of
    Atanh (Tanh x) -> x
    x -> Atanh x
simplify Pi = Pi
simplify E = E

commutativity :: Expr -> Expr -> Expr
commutativity (Add a b) c = Add a (Add b c)
commutativity a (Add b c) = Add (Add a a) b

isPerfectSquare :: Double -> Bool
isPerfectSquare = (==) <$> sqrt <*> (fromIntegral . round . sqrt)

try :: (Expr -> Expr) -> Expr -> Expr
try f x = min x (f x)

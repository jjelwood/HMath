module Core.Types (
    Expr(..),
    Equation(..)
) where

data Expr = Number Double
            | Symbol String
            | Sum [Expr]
            | Prod [Expr]
            | Neg Expr
            | Abs Expr
            | Pow Expr Expr
            | Log Expr Expr
            | Sin Expr
            | Cos Expr
            | Sqrt Expr
            | Asin Expr
            | Acos Expr
            | Atan Expr 
            | Ln Expr
            | Tan Expr
            | Pi
            | E
            deriving (Show, Eq)

-- Exprs can have their complexity compared by a number of metrics
-- for now we'll just use the number of nodes in the tree
-- instance Ord Expr where
--   compare a b = compare (complexity a) (complexity b)

complexity :: Expr -> Int
complexity (Number _) = 1
complexity (Symbol _) = 1
complexity (Sum as) = sum (map (\term -> complexity term + 1) as) - 1
complexity (Prod as) = sum (map (\term -> complexity term + 1) as) - 1
complexity (Neg a) = 1 + complexity a
complexity (Pow a b) = 1 + complexity a + complexity b
complexity (Log a b) = 1 + complexity a + complexity b
complexity (Sin a) = 1 + complexity a
complexity (Cos a) = 1 + complexity a
complexity (Asin a) = 1 + complexity a
complexity (Acos a) = 1 + complexity a
complexity (Atan a) = 1 + complexity a
complexity (Ln a) = 1 + complexity a
complexity (Tan a) = 1 + complexity a
complexity (Abs a) = 1 + complexity a
complexity (Sqrt a) = 1 + complexity a
complexity Pi = 1
complexity E = 1

data Equation = Equation Expr Expr
                deriving (Show, Eq)

instance Num Expr where
    a + b = Sum [a, b]
    a - b = Sum [a, -b]
    a * b = Prod [a, b]
    negate = Neg
    abs = Abs
    signum = undefined
    fromInteger = Number . fromInteger

instance Fractional Expr where
    a / b = Prod [a, Pow b (Number (-1))]
    fromRational = Number . fromRational

instance Floating Expr where 
  pi = Number pi
  exp = Pow (Number $ exp 1)
  log = Log (Number $ exp 1)
  sin = Sin
  cos = Cos
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

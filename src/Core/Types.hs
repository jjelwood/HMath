module Core.Types (
    Expr(..),
    Equation(..),
    operatorPrecedence
) where

data Expr = Number Double
            | Symbol String
            | Sum [Expr]
            | Prod [Expr]
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
instance Ord Expr where
  compare a b = compare (operatorOrdering a) (operatorOrdering b) <>
                compareInstances a b

compareInstances :: Expr -> Expr -> Ordering
compareInstances (Number a) (Number b) = compare a b
compareInstances (Symbol a) (Symbol b) = compare a b
compareInstances (Sum a) (Sum b) = compare a b
compareInstances (Prod a) (Prod b) = compare a b
compareInstances (Abs a) (Abs b) = compare a b
compareInstances (Pow a b) (Pow c d) = compare a c <> compare b d
compareInstances (Log a b) (Log c d) = compare a c <> compare b d
compareInstances (Sin a) (Sin b) = compare a b
compareInstances (Cos a) (Cos b) = compare a b
compareInstances (Sqrt a) (Sqrt b) = compare a b
compareInstances (Asin a) (Asin b) = compare a b
compareInstances (Acos a) (Acos b) = compare a b
compareInstances (Atan a) (Atan b) = compare a b
compareInstances (Ln a) (Ln b) = compare a b
compareInstances (Tan a) (Tan b) = compare a b
compareInstances Pi Pi = EQ
compareInstances E E = EQ
compareInstances _ _ = error "Cannot compare different types of expressions"

-- complexity :: Expr -> Int
-- complexity (Number _) = 1
-- complexity (Symbol _) = 1
-- complexity Pi = 1
-- complexity E = 1
-- complexity (Sum as) = sum (map (\term -> complexity term + 1) as) - 1
-- complexity (Prod as) = sum (map (\term -> complexity term + 1) as) - 1
-- complexity (Pow a b) = 1 + complexity a + complexity b
-- complexity (Log a b) = 1 + complexity a + complexity b
-- complexity (Sin a) = 1 + complexity a
-- complexity (Cos a) = 1 + complexity a
-- complexity (Asin a) = 1 + complexity a
-- complexity (Acos a) = 1 + complexity a
-- complexity (Atan a) = 1 + complexity a
-- complexity (Ln a) = 1 + complexity a
-- complexity (Tan a) = 1 + complexity a
-- complexity (Abs a) = 1 + complexity a
-- complexity (Sqrt a) = 1 + complexity a

operatorOrdering :: Expr -> Int
operatorOrdering (Number _) = 0
operatorOrdering (Symbol _) = 1
operatorOrdering Pi = 2
operatorOrdering E = 3
operatorOrdering (Sum _) = 4
operatorOrdering (Prod _) = 5
operatorOrdering (Pow _ _) = 6
operatorOrdering (Log _ _) = 7
operatorOrdering (Sin _) = 8
operatorOrdering (Cos _) = 9
operatorOrdering (Asin _) = 10
operatorOrdering (Acos _) = 11
operatorOrdering (Atan _) = 12
operatorOrdering (Ln _) = 13
operatorOrdering (Tan _) = 14
operatorOrdering (Abs _) = 15
operatorOrdering (Sqrt _) = 16

operatorPrecedence :: Expr -> Int
operatorPrecedence (Sum _) = 1
operatorPrecedence (Prod _) = 2
operatorPrecedence (Pow _ _) = 3
-- operatorPrecedence (Number _) = 100
-- operatorPrecedence (Symbol _) = 100
-- operatorPrecedence E = 100
-- operatorPrecedence Pi = 100
operatorPrecedence _ = 100


data Equation = Equation Expr Expr
                deriving (Show, Eq)

instance Num Expr where
    a + b = Sum [a, b]
    a - b = Sum [a, -b]
    a * b = Prod [a, b]
    negate = Prod . (:[Number (-1)])
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

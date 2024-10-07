module Core.Types
  ( Expr (..),
    Equation (..),
    operatorPrecedence,
    isNumeric
  )
where

data Expr
  = Number Double
  | Symbol String
  | Sum [Expr]
  | Product [Expr]
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
  | Eq Expr Expr
  | Derivative Expr Expr
  deriving (Show, Eq)

-- Exprs can have their complexity compared by a number of metrics
-- for now we'll just use the number of nodes in the tree
instance Ord Expr where
  compare a b =
    compare (operatorOrdering a) (operatorOrdering b)
      <> compareInstances a b

compareInstances :: Expr -> Expr -> Ordering
compareInstances (Number a) (Number b) = compare a b
compareInstances (Symbol a) (Symbol b) = compare a b
compareInstances (Sum a) (Sum b) = compare a b
compareInstances (Product a) (Product b) = compare a b
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

operatorOrdering :: Expr -> Int
operatorOrdering (Number _) = 0
operatorOrdering (Symbol _) = 1
operatorOrdering Pi = 2
operatorOrdering E = 3
operatorOrdering (Sum _) = 4
operatorOrdering (Product _) = 5
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
operatorOrdering (Derivative _ _) = 17
operatorOrdering (Eq _ _) = 18

operatorPrecedence :: Expr -> Int
operatorPrecedence (Sum _) = 1
operatorPrecedence (Product _) = 2
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
  a * b = Product [a, b]
  negate = Product . (: [Number (-1)])
  abs = Abs
  signum = undefined
  fromInteger = Number . fromInteger

instance Fractional Expr where
  a / b = Product [a, Pow b (Number (-1))]
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

isNumeric :: [Expr] -> Expr -> Bool
isNumeric vars expr = notElem expr vars && case expr of
  Number _ -> True
  Symbol _ -> True
  E -> True
  Pi -> True
  Eq x y -> isNum x && isNum y
  Derivative _ _ -> False
  Sum xs -> all isNum xs
  Product xs -> all isNum xs
  Abs x -> isNum x
  Pow x y -> isNum x && isNum y
  Log x y -> isNum x && isNum y
  Sin x -> isNum x
  Cos x -> isNum x
  Sqrt x -> isNum x
  Asin x -> isNum x
  Acos x -> isNum x
  Atan x -> isNum x
  Ln x -> isNum x
  Tan x -> isNum x
  where
    isNum = isNumeric vars

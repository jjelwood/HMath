module Core.Rewrite
  ( eval,
    simplify,
    simplifyWith,
    getCoefficient,
    subst
  )
where

import Core.Types (Expr (..))
import Core.Derivatives (differentiate)
import Data.List (sort)
import Data.Either (fromLeft, fromRight, isLeft)

-- Evaluate an expression
evalExpr :: Expr -> Either String Double
evalExpr e = case e of
  Number x -> Right x
  Symbol x -> Left $ "Cannot evaluate symbol " ++ x
  Sum as -> sum <$> mapM evalExpr as
  Product as -> product <$> mapM evalExpr as
  Eq _ _ -> Left "Cannot evaluate equation"
  Derivative _ _ -> Left "Cannot evaluate derivative"
  Abs a -> abs <$> evalExpr a
  Pow a b -> (**) <$> evalExpr a <*> evalExpr b
  Log a b -> logBase <$> evalExpr a <*> evalExpr b
  Ln a -> log <$> evalExpr a
  Sin a -> sin <$> evalExpr a
  Cos a -> cos <$> evalExpr a
  Tan a -> tan <$> evalExpr a
  Sqrt a -> sqrt <$> evalExpr a
  Asin a -> asin <$> evalExpr a
  Acos a -> acos <$> evalExpr a
  Atan a -> atan <$> evalExpr a
  Pi -> Right pi
  E -> Right $ exp 1
  Subst expr var val -> evalExpr $ simplify $ subst var val expr
  NSolve expr var -> case nsolve expr var of
    Number x -> Right x
    _ -> Left "Cannot solve expression"
  Error s -> Left s

-- Substitute a variable with a value in an expression
subst :: Expr -> Expr -> Expr -> Expr
subst expr var val = simplifyWith (Just (var, val)) expr

-- Evaluate an expression with a variable substituted with a number
eval :: Expr -> Expr -> Double -> Either String Double
eval expr var val = evalExpr $ subst expr var (Number val)

-- Simplify an expression
simplify :: Expr -> Expr
simplify = simplifyWith Nothing

-- Simplify an expression with a set of variable substitutions
simplifyWith :: Maybe (Expr, Expr) -> Expr -> Expr
simplifyWith (Just (var, val)) expr | var == expr = val
simplifyWith _ n@(Number _) = n
simplifyWith _ s@(Symbol _) = s
simplifyWith _ Pi = Pi
simplifyWith _ E = E
simplifyWith vars (Eq e1 e2) = Eq (simplifyWith vars e1) (simplifyWith vars e2)
simplifyWith vars (Derivative x e) = simplifyWith vars $ differentiate x e
simplifyWith vars (Sum as) = case filter (/= Number 0) $ map (simplifyWith vars) as of
  xs | any (\case Error _ -> True; _ -> False) xs -> head $ filter (\case Error _ -> True; _ -> False) xs
  [] -> Number 0
  [x] -> x
  xs ->
    combineLikeTerms $ -- combine like terms -- combine like terms
       -- combine like terms
      map (\case Product ys -> Product $ sort ys; y -> y) $ -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same
        concatMap (\case Sum ys -> ys; y -> [y]) xs -- pull up nested sums
simplifyWith vars (Product as) = case filter (/= Number 1) $ map (simplifyWith vars) as of
  xs | any (\case Error _ -> True; _ -> False) xs -> head $ filter (\case Error _ -> True; _ -> False) xs
  [] -> Number 1
  [x] -> x
  xs ->
    combineCommonFactors $ -- combine common factors
      concatMap (\case Product ys -> ys; Pow (Product ys) e -> map (`Pow` e) ys; y -> [y]) xs -- pull up nested products and expand powers
simplifyWith vars (Abs a) = case simplifyWith vars a of
  Error e -> Error e
  Number x -> Number (abs x)
  Abs (Abs x) -> Abs x
  x -> Abs x
simplifyWith vars (Pow a b) = case (simplifyWith vars a, simplifyWith vars b) of
  (Error e, _) -> Error e
  (_, Error e) -> Error e
  (Number 0, Number 0) -> error "0^0 is undefined"
  (_, Number 0) -> Number 1
  (x, Number 1) -> x
  (Number 0, _) -> Number 0
  (Number 1, _) -> Number 1
  (Pow x y, z) -> Pow x $ simplifyWith vars $ y * z
  (base, Log base' x) | base == base' -> x
  (x, y) -> Pow x y
simplifyWith vars (Log a b) = case (simplifyWith vars a, simplifyWith vars b) of
  (Error e, _) -> Error e
  (_, Error e) -> Error e
  (_, Number 0) -> error "log 0 is undefined"
  (_, Number 1) -> Number 0
  (base, Pow base' x) | base == base' -> x
  (E, x) -> Ln x
  (x, y) -> Log x y
simplifyWith vars (Ln a) = case simplifyWith vars a of
  Error e -> Error e
  Number x -> Number (log x)
  x -> Ln x
simplifyWith vars (Sin a) = case simplifyWith vars a of
  Error e -> Error e
  Sin (Asin x) -> x
  x -> Sin x
simplifyWith vars (Cos a) = case simplifyWith vars a of
  Error e -> Error e
  Cos (Acos x) -> x
  x -> Cos x
simplifyWith vars (Tan a) = case simplifyWith vars a of
  Error e -> Error e
  Tan (Atan x) -> x
  x -> Tan x
simplifyWith vars (Sqrt a) = case simplifyWith vars a of
  Error e -> Error e
  Number x | isPerfectSquare x -> Number (sqrt x)
  Sqrt (Pow x (Number 2)) -> x
  x -> Sqrt x
simplifyWith vars (Asin a) = case simplifyWith vars a of
  Error e -> Error e
  Asin (Sin x) -> x
  x -> Asin x
simplifyWith vars (Acos a) = case simplifyWith vars a of
  Error e -> Error e
  Acos (Cos x) -> x
  x -> Acos x
simplifyWith vars (Atan a) = case simplifyWith vars a of
  Error e -> Error e
  Atan (Tan x) -> x 
  x -> Atan x
simplifyWith vars (Subst expr var val) = simplifyWith (Just (var, val)) $ simplifyWith vars expr
simplifyWith vars (NSolve expr var) = case nsolve inner var of
  Error e -> Error e
  x -> x
  where
    inner = simplifyWith vars expr
simplifyWith _ (Error s) = Error s

-- Separate the coefficient from the rest of the expression, returns the rest of the expression as a product
getCoefficient :: Expr -> (Double, Expr)
getCoefficient (Product as) = (product $ map (\case Number x -> x; _ -> 1) as, Product $ filter (\case Number _ -> False; _ -> True) as)
getCoefficient (Number x) = (x, Product [1])
getCoefficient x = (1, Product [x])

getExprC :: (Double, Expr) -> Expr
getExprC (0, _) = Number 0
getExprC (1, Product [t]) = t
getExprC (1, Product ts) = Product ts
getExprC (c, Product ts) = Product $ Number c : ts
getExprC (c, t) = Product [Number c, t]

getExponent :: Expr -> (Double, Expr)
getExponent (Pow b (Number e)) = (e, b)
getExponent x = (1, x)

getExprE :: (Double, Expr) -> Expr
getExprE (0, _) = Number 1
getExprE (1, t) = t
getExprE (e, t) = Pow t $ Number e

combineLikeTerms :: [Expr] -> Expr
combineLikeTerms ts = case filter (/= Number 0) $
  map
    (multIfNumProduct . getExprC)
    ( foldl addTerm [] $
        map getCoefficient ts
    ) of
  [] -> Number 0
  [x] -> x
  xs -> Sum $ sort xs
  -- xs -> Sum $ sortBy (flip compare) xs
  where
    addTerm [] (coefficient, term) = [(coefficient, term)]
    addTerm ((c, t) : rest) (coefficient, term)
      | t == term = (c + coefficient, term) : rest
      | otherwise = (c, t) : addTerm rest (coefficient, term)

multIfNumProduct :: Expr -> Expr
multIfNumProduct (Product xs)
  | isNumProduct xs = Number $ product $ map (\case Number x -> x; _ -> error "Not a number") xs
  | otherwise = Product xs
multIfNumProduct x = x

isNumProduct :: [Expr] -> Bool
isNumProduct = all (\case Number _ -> True; _ -> False)

combineCommonFactors :: [Expr] -> Expr
combineCommonFactors fs = case filter (/= Number 1) $ map getExprE $ foldl multFactor [] $ map getExponent fs of
  [] -> Number 1
  [x] -> x
  xs -> Product $ mergeConstants $ sort xs
  where
    multFactor factors (power, Number x) = (power, Number x) : factors
    multFactor [] (power, factor) = [(power, factor)]
    multFactor ((p, f) : rest) (power, factor)
      | f == factor = (p + power, factor) : rest
      | otherwise = (p, f) : multFactor rest (power, factor)
    mergeConstants exprs = case exprs of
      [] -> []
      [x] -> [x]
      (Number a) : (Number b) : rest -> mergeConstants $ Number (a * b) : rest
      xs -> xs

isPerfectSquare :: Double -> Bool
isPerfectSquare = (==) <$> sqrt <*> (fromIntegral . round . sqrt)

newtonsMethod :: Double -> Int -> Expr -> Expr -> Double -> Expr
newtonsMethod tolerance count f x x0 
    | isLeft x1 = Error $ "Newton's method failed to evaluate: " <> x1err
    | count == 0 = Error "Newton's method did not converge"
    | abs (x1res - x0) < tolerance = Eq x $ Number x1res
    | otherwise = newtonsMethod tolerance (count - 1) f x x1res
    where
        f_x0 = eval f x x0
        f'_x0 = eval (differentiate x f) x x0
        x1 = case (f_x0, f'_x0) of
            (Right fx0, Right f'x0) -> Right $ x0 - fx0 / f'x0
            (Left err, _) -> Left err
            (_, Left err) -> Left err
        x1res = fromRight (error "x1 is right") x1
        x1err = fromLeft (error "x1 is left") x1

nsolveSeed :: Expr -> Expr -> Double -> Expr
nsolveSeed = newtonsMethod 0.00001 100 

nsolve :: Expr -> Expr -> Expr
nsolve (Eq lhs rhs) x = nsolveSeed (lhs - rhs) x 0
nsolve f x = nsolveSeed f x 0

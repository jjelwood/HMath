module Core.Rewrite
  ( eval,
    simplify,
    simplifyWith,
    getCoefficient,
  )
where

import Core.Types (Expr (..))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

eval :: Expr -> Expr
eval e = case e of
  Number x -> Number x
  Symbol x -> Symbol x
  Sum as -> sum $ map eval as
  Prod as -> product $ map eval as
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
  Pi -> Number pi
  E -> Number (exp 1)

simplify :: Expr -> Expr
simplify = simplifyWith M.empty

simplifyWith :: Map String Expr -> Expr -> Expr
simplifyWith _ (Number x) = Number x
simplifyWith vars s@(Symbol x) = fromMaybe s $ vars M.!? x
simplifyWith vars (Sum as) = case filter (/= Number 0) $ map (simplifyWith vars) as of
  [] -> Number 0
  [x] -> x
  xs ->
    combineLikeTerms $ -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms -- combine like terms -- combine like terms
    -- combine like terms
    -- combine like terms
    -- combine like terms -- combine like terms
    -- combine like terms
      map (\case Prod ys -> Prod $ sort ys; y -> y) $ -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same -- sort, i.e. so that xy and yx are considered the same
        concatMap (\case Sum ys -> ys; y -> [y]) xs -- pull up nested sums
simplifyWith vars (Prod as) = case filter (/= Number 1) $ map (simplifyWith vars) as of
  [] -> Number 1
  [x] -> x
  xs ->
    combineCommonFactors $ -- combine common factors
      concatMap (\case Prod ys -> ys; Pow (Prod ys) e -> map (`Pow` e) ys; y -> [y]) xs -- pull up nested products and expand powers
simplifyWith vars (Abs a) = case simplifyWith vars a of
  Number x -> Number (abs x)
  Abs (Abs x) -> Abs x
  x -> Abs x
simplifyWith vars (Pow a b) = case (simplifyWith vars a, simplifyWith vars b) of
  (Number 0, Number 0) -> error "0^0 is undefined"
  (_, Number 0) -> Number 1
  (x, Number 1) -> x
  (Number 0, _) -> Number 0
  (Number 1, _) -> Number 1
  (Pow x y, z) -> Pow x $ simplifyWith vars $ y * z
  (base, Log base' x) | base == base' -> x
  (x, y) -> Pow x y
simplifyWith vars (Log a b) = case (simplifyWith vars a, simplifyWith vars b) of
  (_, Number 0) -> error "log 0 is undefined"
  (_, Number 1) -> Number 0
  (base, Pow base' x) | base == base' -> x
  (E, x) -> Ln x
  (x, y) -> Log x y
simplifyWith vars (Ln a) = case simplifyWith vars a of
  Number x -> Number (log x)
  x -> Ln x
simplifyWith vars (Sin a) = case simplifyWith vars a of
  Sin (Asin x) -> x
  x -> Sin x
simplifyWith vars (Cos a) = case simplifyWith vars a of
  Cos (Acos x) -> x
  x -> Cos x
simplifyWith vars (Tan a) = case simplifyWith vars a of
  Tan (Atan x) -> x
  x -> Tan x
simplifyWith vars (Sqrt a) = case simplifyWith vars a of
  Number x | isPerfectSquare x -> Number (sqrt x)
  Sqrt (Pow x (Number 2)) -> x
  x -> Sqrt x
simplifyWith vars (Asin a) = case simplifyWith vars a of
  Asin (Sin x) -> x
  x -> Asin x
simplifyWith vars (Acos a) = case simplifyWith vars a of
  Acos (Cos x) -> x
  x -> Acos x
simplifyWith vars (Atan a) = case simplifyWith vars a of
  Atan (Tan x) -> x
  x -> Atan x
simplifyWith _ Pi = Pi
simplifyWith _ E = E

-- Separate the coefficient from the rest of the expression, returns the rest of the expression as a product
getCoefficient :: Expr -> (Double, Expr)
getCoefficient (Prod as) = (product $ map (\case Number x -> x; _ -> 1) as, Prod $ filter (\case Number _ -> False; _ -> True) as)
getCoefficient (Number x) = (x, Prod [1])
getCoefficient x = (1, Prod [x])

getExprC :: (Double, Expr) -> Expr
getExprC (0, _) = Number 0
getExprC (1, Prod [t]) = t
getExprC (1, Prod ts) = Prod ts
getExprC (c, Prod ts) = Prod $ Number c : ts
getExprC (c, t) = Prod [Number c, t]

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
    (multIfNumProd . getExprC)
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

multIfNumProd :: Expr -> Expr
multIfNumProd (Prod xs)
  | isNumProd xs = Number $ product $ map (\case Number x -> x; _ -> error "Not a number") xs
  | otherwise = Prod xs
multIfNumProd x = x

isNumProd :: [Expr] -> Bool
isNumProd = all (\case Number _ -> True; _ -> False)

combineCommonFactors :: [Expr] -> Expr
combineCommonFactors fs = case filter (/= Number 1) $ map getExprE $ foldl multFactor [] $ map getExponent fs of
  [] -> Number 1
  [x] -> x
  xs -> Prod $ mergeConstants $ sort xs
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

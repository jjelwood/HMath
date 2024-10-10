module Core.Derivatives (
    differentiate
) where

import Core.Types

differentiate :: Expr -> Expr -> Expr
differentiate s f = if s == f then 1 else case f of
    Symbol _ -> 0
    Number _ -> 0
    E -> 0
    Pi -> 0
    Sum fs -> Sum $ map d fs
    Product fs -> foldl1 (\acc expr -> expr * d acc + d expr * acc) fs
    Pow b n | isNumeric [s] n -> chain b $ n * Pow b (n - 1)
    Pow E e -> chain e $ Pow E e
    Pow b e -> d $ Pow E (e * Ln b)
    Log x y -> d $ Product [Ln x, recip (Ln y)]
    Ln x -> chain x $ recip x
    Sqrt x -> d $ Pow x 0.5
    Abs _ -> undefined
    Sin x -> chain x $ Cos x
    Cos x -> chain x $ -Sin x
    Tan x -> chain x $ Pow (Cos x) (-2)
    Asin x -> chain x $ Pow (1 - Pow x 2) (-0.5)
    Acos x -> chain x $ -Pow (1 - Pow x 2) (-0.5)
    Atan x -> chain x $ recip (Pow x 2 + 1)
    Eq lhs rhs -> Eq (d lhs) (d rhs)
    Subst {} -> Error "Cannot differentiate a substitution"
    Derivative _ _ -> Error "Cannot differentiate a derivative"
    NSolve _ _ -> 0
    Error e -> Error e
    where
        d = differentiate s
        chain arg expr' = expr' * d arg


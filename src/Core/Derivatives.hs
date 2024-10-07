module Core.Derivatives (
    differentiate
) where

import Core.Types
import Debug.Trace (traceShow)

differentiate :: Expr -> Expr -> Expr
differentiate s f = traceShow f $ if s == f then 1 else case f of
    Symbol _ -> 0
    Number _ -> 0
    E -> 0
    Pi -> 0
    Eq _ _ -> error "Cannot differentiate an equation"
    Derivative _ _ -> error "Cannot differentiate a derivative"
    Sum fs -> Sum $ map d fs
    Product fs -> foldl1 (\acc expr -> Sum [Product [expr, d acc], Product [d expr, acc]]) fs
    Pow b n | isNumeric [s] n -> chain b $ Product [n, Pow b (Sum [n, Number (-1)])]
    Pow E e -> chain e $ Pow E e
    Pow b e -> d $ Pow E (Product [Ln b, e])
    Log x y -> d $ Product [Ln x, Pow (Ln y) (-1)]
    Ln x -> chain x $ Pow x (Number (-1))
    Sqrt x -> d $ Pow x (Number 0.5)
    Abs x -> d $ Pow (Pow x 2) 0.5
    Sin x -> chain x $ Cos x
    Cos x -> chain x $ Product [Number (-1), Sin x]
    Tan x -> chain x $ Pow (Cos x) (-2)
    Asin _ -> undefined
    Acos _ -> undefined
    Atan _ -> undefined
    where
        d = differentiate s 
        chain arg expr' = Product [expr', d arg]
    

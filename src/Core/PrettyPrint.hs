module Core.PrettyPrint
  ( prettyPrint,
    prettyPrintLatex,
  )
where

import Core.Types (Expr (..), operatorPrecedence)
import Data.List (intercalate)

prettyPrint :: Expr -> String
prettyPrint (Number x) = if x == fromIntegral (round x) then show $ round x else show x
prettyPrint (Symbol x) = x
prettyPrint (Eq a b) = prettyPrint a <> " = " <> prettyPrint b
prettyPrint (Derivative x f) = "d/d" <> prettyPrint x <> "(" <> prettyPrint f <> ")"
prettyPrint e@(Sum as) = intercalate " + " $ map (bracketIfLowerPrecedence prettyPrint e) as
prettyPrint e@(Product as) = intercalate " * " $ map (bracketIfLowerPrecedence prettyPrint e) as
prettyPrint (Abs a) = "|" <> prettyPrint a <> "|"
prettyPrint e@(Pow a b) = bracketIfLowerPrecedence prettyPrint e a <> " ^ " <> bracketIfLowerPrecedence prettyPrint e b
prettyPrint e@(Log a b) = "log_" <> bracketIfLowerPrecedence prettyPrint e a <> "(" <> prettyPrint b <> ")"
prettyPrint (Sin a) = "sin(" <> prettyPrint a <> ")"
prettyPrint (Cos a) = "cos(" <> prettyPrint a <> ")"
prettyPrint (Sqrt a) = "sqrt(" <> prettyPrint a <> ")"
prettyPrint (Asin a) = "asin(" <> prettyPrint a <> ")"
prettyPrint (Acos a) = "acos(" <> prettyPrint a <> ")"
prettyPrint (Atan a) = "atan(" <> prettyPrint a <> ")"
prettyPrint (Ln a) = "ln(" <> prettyPrint a <> ")"
prettyPrint (Tan a) = "tan(" <> prettyPrint a <> ")"
prettyPrint (Subst a b c) = "(" <> prettyPrint a <> ") /. (" <> prettyPrint b <> " -> " <> prettyPrint c <> ")"
prettyPrint (NSolve a b) = "NSolve(" <> prettyPrint a <> ", " <> prettyPrint b <> ")"
prettyPrint Pi = "pi"
prettyPrint E = "e"
prettyPrint (Error s) = "Error: " <> s

bracketIfLowerPrecedence :: (Expr -> String) -> Expr -> Expr -> String
bracketIfLowerPrecedence f a b = if operatorPrecedence b < operatorPrecedence a then "(" <> f b <> ")" else f b

prettyPrintLatex :: Expr -> String
prettyPrintLatex (Number x) = show x
prettyPrintLatex (Symbol x) = x
prettyPrintLatex e@(Sum as) = intercalate " + " $ map (bracketIfLowerPrecedence prettyPrint e) as
prettyPrintLatex e@(Product as) = intercalate " \\cdot " $ map (bracketIfLowerPrecedence prettyPrint e) as
prettyPrintLatex (Eq a b) = prettyPrintLatex a <> " = " <> prettyPrintLatex b
prettyPrintLatex (Derivative x f) = "\\frac{d}{d" <> prettyPrintLatex x <> "}\\left(" <> prettyPrintLatex f <> "\\right)"
prettyPrintLatex (Abs a) = "\\left|" <> prettyPrintLatex a <> "\\right|"
prettyPrintLatex e@(Pow a b) = bracketIfLowerPrecedence prettyPrintLatex e a <> "^{" <> prettyPrintLatex b <> "}"
prettyPrintLatex (Log a b) = "\\log_{" <> prettyPrintLatex a <> "}\\left(" <> prettyPrintLatex b <> "\\right)"
prettyPrintLatex (Sin a) = "\\sin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Cos a) = "\\cos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Sqrt a) = "\\sqrt{" <> prettyPrintLatex a <> "}"
prettyPrintLatex (Asin a) = "\\arcsin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Acos a) = "\\arccos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Atan a) = "\\arctan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Ln a) = "\\ln\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Tan a) = "\\tan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Subst a b c) = "\\left(" <> prettyPrintLatex a <> "\\right) /. \\left(" <> prettyPrintLatex b <> " -> " <> prettyPrintLatex c <> "\\right)"
prettyPrintLatex (NSolve a b) = "\\text{NSolve}\\left(" <> prettyPrintLatex a <> ", " <> prettyPrintLatex b <> "\\right)"
prettyPrintLatex Pi = "\\pi"
prettyPrintLatex E = "e"
prettyPrintLatex (Error s) = "\\text{Error: " <> s <> "}"

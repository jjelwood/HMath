module Core.PrettyPrint (
    prettyPrint,
    prettyPrintLatex
) where

import Core.Types ( Expr(..) )

prettyPrint :: Expr -> String
prettyPrint (Number x) = show x
prettyPrint (Symbol x) = x
prettyPrint (Add a b) = prettyPrint a <> " + " <> prettyPrint b
prettyPrint (Sub a b) = prettyPrint a <> " - " <> prettyPrint b
prettyPrint (Mul a b) = prettyPrint a <> " * " <> prettyPrint b
prettyPrint (Div a b) = prettyPrint a <> " / " <> prettyPrint b
prettyPrint (Neg a) = "-" <> prettyPrint a
prettyPrint (Abs a) = "|" <> prettyPrint a <> "|"
prettyPrint (Pow a b) = prettyPrint a <> " ^ " <> prettyPrint b
prettyPrint (Log a b) = "log_" <> prettyPrint a <> "(" <> prettyPrint b <> ")"
prettyPrint (Sin a) = "sin(" <> prettyPrint a <> ")"
prettyPrint (Cos a) = "cos(" <> prettyPrint a <> ")"
prettyPrint (Sqrt a) = "sqrt(" <> prettyPrint a <> ")"
prettyPrint (Asin a) = "asin(" <> prettyPrint a <> ")"
prettyPrint (Acos a) = "acos(" <> prettyPrint a <> ")"
prettyPrint (Atan a) = "atan(" <> prettyPrint a <> ")"
prettyPrint (Sinh a) = "sinh(" <> prettyPrint a <> ")"
prettyPrint (Cosh a) = "cosh(" <> prettyPrint a <> ")"
prettyPrint (Tanh a) = "tanh(" <> prettyPrint a <> ")"
prettyPrint (Asinh a) = "asinh(" <> prettyPrint a <> ")"
prettyPrint (Acosh a) = "acosh(" <> prettyPrint a <> ")"
prettyPrint (Atanh a) = "atanh(" <> prettyPrint a <> ")"
prettyPrint (Ln a) = "ln(" <> prettyPrint a <> ")"
prettyPrint (Tan a) = "tan(" <> prettyPrint a <> ")"
prettyPrint Pi = "pi"
prettyPrint E = "e"

prettyPrintLatex :: Expr -> String
prettyPrintLatex (Number x) = show x
prettyPrintLatex (Symbol x) = x
prettyPrintLatex (Add a b) = prettyPrintLatex a <> " + " <> prettyPrintLatex b
prettyPrintLatex (Sub a b) = prettyPrintLatex a <> " - " <> prettyPrintLatex b
prettyPrintLatex (Mul a b) = prettyPrintLatex a <> " \\cdot " <> prettyPrintLatex b
prettyPrintLatex (Div a b) = "\\frac{" <> prettyPrintLatex a <> "}{" <> prettyPrintLatex b <> "}"
prettyPrintLatex (Neg a) = "-" <> prettyPrintLatex a
prettyPrintLatex (Abs a) = "\\left|" <> prettyPrintLatex a <> "\\right|"
prettyPrintLatex (Pow a b) = prettyPrintLatex a <> "^{" <> prettyPrintLatex b <> "}"
prettyPrintLatex (Log a b) = "\\log_{" <> prettyPrintLatex a <> "}\\left(" <> prettyPrintLatex b <> "\\right)"
prettyPrintLatex (Sin a) = "\\sin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Cos a) = "\\cos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Sqrt a) = "\\sqrt{" <> prettyPrintLatex a <> "}"
prettyPrintLatex (Asin a) = "\\arcsin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Acos a) = "\\arccos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Atan a) = "\\arctan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Sinh a) = "\\sinh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Cosh a) = "\\cosh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Tanh a) = "\\tanh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Asinh a) = "\\arsinh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Acosh a) = "\\arcosh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Atanh a) = "\\artanh\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Ln a) = "\\ln\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Tan a) = "\\tan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex Pi = "\\pi"
prettyPrintLatex E = "e"

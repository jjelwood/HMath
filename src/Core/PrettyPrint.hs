module Core.PrettyPrint (
    prettyPrint,
    prettyPrintLatex
) where

import Core.Types ( Expr(..) )
import Data.List (intercalate)

prettyPrint :: Expr -> String
prettyPrint (Number x) = show x
prettyPrint (Symbol x) = x
prettyPrint (Sum as) = intercalate " + " $ map (\a -> "(" <> prettyPrint a <> ")") as
prettyPrint (Prod as) = intercalate " * " $ map (\a -> "(" <> prettyPrint a <> ")") as
prettyPrint (Abs a) = "|" <> prettyPrint a <> "|"
prettyPrint (Pow a b) = prettyPrint a <> " ^ " <> prettyPrint b
prettyPrint (Log a b) = "log_" <> prettyPrint a <> "(" <> prettyPrint b <> ")"
prettyPrint (Sin a) = "sin(" <> prettyPrint a <> ")"
prettyPrint (Cos a) = "cos(" <> prettyPrint a <> ")"
prettyPrint (Sqrt a) = "sqrt(" <> prettyPrint a <> ")"
prettyPrint (Asin a) = "asin(" <> prettyPrint a <> ")"
prettyPrint (Acos a) = "acos(" <> prettyPrint a <> ")"
prettyPrint (Atan a) = "atan(" <> prettyPrint a <> ")"
prettyPrint (Ln a) = "ln(" <> prettyPrint a <> ")"
prettyPrint (Tan a) = "tan(" <> prettyPrint a <> ")"
prettyPrint Pi = "pi"
prettyPrint E = "e"

prettyPrintLatex :: Expr -> String
prettyPrintLatex (Number x) = show x
prettyPrintLatex (Symbol x) = x
prettyPrintLatex (Sum as) = intercalate " + " $ map (\a -> "(" <> prettyPrintLatex a <> ")") as
prettyPrintLatex (Prod as) = intercalate " \\cdot " $ map (\a -> "(" <> prettyPrintLatex a <> ")") as
prettyPrintLatex (Abs a) = "\\left|" <> prettyPrintLatex a <> "\\right|"
prettyPrintLatex (Pow a b) = prettyPrintLatex a <> "^{" <> prettyPrintLatex b <> "}"
prettyPrintLatex (Log a b) = "\\log_{" <> prettyPrintLatex a <> "}\\left(" <> prettyPrintLatex b <> "\\right)"
prettyPrintLatex (Sin a) = "\\sin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Cos a) = "\\cos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Sqrt a) = "\\sqrt{" <> prettyPrintLatex a <> "}"
prettyPrintLatex (Asin a) = "\\arcsin\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Acos a) = "\\arccos\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Atan a) = "\\arctan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Ln a) = "\\ln\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex (Tan a) = "\\tan\\left(" <> prettyPrintLatex a <> "\\right)"
prettyPrintLatex Pi = "\\pi"
prettyPrintLatex E = "e"

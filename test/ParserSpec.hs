module ParserSpec where

import Test.HUnit
import Core.Parser
import Core.Types

testParse :: String -> Expr -> Test
testParse input expected = 
    input ~: 
        case parseExpr input of
            Left err -> assertFailure $ "Parse error: " ++ err
            Right (Left err) -> assertFailure $ "Parse error: " ++ err
            Right (Right expr) -> assertEqual "" expected expr

testList :: Test
testList = TestList [
    testParse "1" (Number 1),
    testParse "x" (Symbol "x"),
    testParse "x + 1" (Sum [Symbol "x", Number 1]),
    testParse "sin(x)" (Sin (Symbol "x")),
    testParse "cos(x)" (Cos (Symbol "x")),
    testParse "log(x, y)" (Log (Symbol "x") (Symbol "y")),
    testParse "sin(x) + cos(y)" (Sum [Sin (Symbol "x"), Cos (Symbol "y")]),
    testParse "x * y" (Prod [Symbol "x", Symbol "y"]),
    testParse "x * y + z" (Sum [Prod [Symbol "x", Symbol "y"], Symbol "z"]),
    testParse "x * (y + z)" (Prod [Symbol "x", Sum [Symbol "y", Symbol "z"]]),
    testParse "x + y * z" (Sum [Symbol "x", Prod [Symbol "y", Symbol "z"]]),
    ]


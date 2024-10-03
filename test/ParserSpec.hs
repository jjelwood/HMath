module ParserSpec (
    testList
) where

import Test.HUnit
import Core.Parser
import Core.Types

testParse :: String -> Expr -> Test
testParse input expected = 
    input ~: 
        case runParser input of
            Left err -> assertFailure $ "Parse error: " ++ err
            Right (Left err) -> assertFailure $ "Parse error: " ++ err
            Right (Right expr) -> assertEqual "" expected expr

testFail :: String -> Test
testFail input = 
    input ~: 
        case runParser input of
            Left _ -> return ()
            Right (Left _) -> return ()
            Right (Right _) -> assertFailure "Expected parse error"

passList :: Test
passList = TestList [
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
    testParse "2x" (Prod [Number 2, Symbol "x"])
    ]

failList :: Test
failList = TestList [
    testFail "1 +",
    testFail "1 + 2 +",
    testFail "-",
    testFail "sin",
    testFail "sin()",
    testFail "sin(x, y)",
    testFail "sin(x) +",
    testFail "sin(x) + cos"
    ]

testList :: Test
testList = TestList [passList,  failList]


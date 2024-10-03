import Test.HUnit
import qualified ParserSpec as Parser

main :: IO Counts
main = runTestTT $ TestList [
    Parser.testList
  ]


import Test.HUnit
import Test.QuickCheck
import qualified ParserSpec as Parser

main :: IO Counts
main = runTestTT $ TestList [
    Parser.testList
  ]


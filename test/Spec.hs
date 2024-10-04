import qualified ParserSpec as Parser
import Test.HUnit

main :: IO Counts
main =
  runTestTT $
    TestList
      [ Parser.testList
      ]

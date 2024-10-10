import qualified ParserSpec as Parser
import Test.HUnit

main :: IO Counts
main = do
  putStrLn "Running tests..."
  runTestTT $
    TestList
      [ Parser.testList
      ]

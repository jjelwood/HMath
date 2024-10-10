module Main (main) where

import Control.Monad (forever)
import Core.Parser (runParser)
import Core.PrettyPrint (prettyPrint)
import Core.Rewrite (simplify)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  runShell

runShell :: IO ()
runShell = forever $ do
  putStr "HMath> "
  hFlush stdout
  input <- getLine
  case runParser input of
    Left err -> putStrLn err
    Right expr -> do
      putStrLn $ prettyPrint $ simplify expr

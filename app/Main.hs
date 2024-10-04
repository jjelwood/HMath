module Main (main) where

import Control.Monad (forever)
import Core.Parser (runParser)
import Core.PrettyPrint (prettyPrint)
import Core.Rewrite (simplifyWith)
import Core.Types (Expr)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let initialMap = M.empty
  runShell initialMap

runShell :: Map String Expr -> IO ()
runShell vars = forever $ do
  putStr "HMath> "
  hFlush stdout
  input <- getLine
  if '=' `elem` input
    then do
      let (var, expr) = break (== '=') input
      case runParser $ trimL $ tail expr of
        Left err -> putStrLn err
        Right expr' -> runShell (M.insert (trimR var) expr' vars)
    else case runParser input of
      Left err -> putStrLn err
      Right expr -> do
        -- print expr
        -- print $ simplifyWith vars expr
        -- putStrLn $ prettyPrint expr
        putStrLn $ prettyPrint $ simplifyWith vars expr

trimR :: String -> String
trimR = reverse . dropWhile (== ' ') . reverse

trimL :: String -> String
trimL = dropWhile (== ' ')

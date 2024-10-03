module Main (main) where

import Core.Rewrite ( simplifyWith )
import Core.PrettyPrint ( prettyPrint )
import Core.Parser ( runParser )
import Data.Map ( Map )
import qualified Data.Map as M
import Core.Types (Expr)
import Control.Monad (forever)
import System.IO ( hFlush, stdout )

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
        Right (Left err) -> putStrLn err
        Right (Right expr') -> runShell (M.insert (trimR var) expr' vars)
    else case runParser input of
      Left err -> putStrLn err
      Right (Left err) -> putStrLn err
      Right (Right expr) -> do
        print expr
        print $ simplifyWith vars expr
        putStrLn $ prettyPrint expr
        putStrLn $ prettyPrint $ simplifyWith vars expr

trimR :: String -> String
trimR = reverse . dropWhile (== ' ') . reverse

trimL :: String -> String
trimL = dropWhile (== ' ')

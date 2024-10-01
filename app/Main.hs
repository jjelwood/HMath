module Main (main) where

import Core.Parser ( exprParser )
import Core.Rewrite ( simplifyWith )
import Core.PrettyPrint ( prettyPrint )
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Data.Map ( Map )
import qualified Data.Map as M
import Core.Types (Expr)
import Control.Monad (forever)
import System.IO ( hFlush, stdout )
import Debug.Trace ( traceShowId )

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
        -- putStrLn $ prettyPrint expr
        putStrLn $ prettyPrint $ simplifyWith (traceShowId vars) expr

trimR :: String -> String
trimR = reverse . dropWhile (== ' ') . reverse

trimL :: String -> String
trimL = dropWhile (== ' ')


-- Run the parser on the input
runParser :: String -> Either String (Either String Expr)
runParser = A.parseOnly exprParser . T.pack

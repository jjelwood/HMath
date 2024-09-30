module Main (main) where

import Core.Parser ( exprParser )
import Core.Rewrite ( simplify )
import Core.PrettyPrint ( prettyPrint )
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

data Options = Options
  { getInput   :: String  -- Input expression or file
  , verbose :: Bool    -- Verbosity flag
  } deriving Show

inputParser :: O.Parser String
inputParser = strOption
  ( long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "The input expression to be evaluated" )

-- Parser for the verbosity flag
verboseParser :: O.Parser Bool
verboseParser = switch
  ( long "verbose"
  <> short 'v'
  <> help "Show steps taken during computation" )

-- Combine the parsers into one for Options
optionsParser :: O.Parser Options
optionsParser = Options
  <$> inputParser
  <*> verboseParser

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  runWithOptions opts

-- Wrap the parser with a description
optsParserInfo :: O.ParserInfo Options
optsParserInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Run HMath with the given input"
  <> header "HMath CLI - a simple interface to interact with the solver" )

-- Use the parsed options to run the program
runWithOptions :: Options -> IO ()
runWithOptions (Options input _) = do
  putStrLn $ "Input: " ++ input
  print (runParser input)

-- Run the parser on the input
runParser :: String -> Either String String
runParser input = A.parseOnly (prettyPrint . simplify <$> exprParser) (T.pack input)


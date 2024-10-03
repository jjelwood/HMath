module Core.Parser (
  exprParser, runParser
) where

import Data.Attoparsec.Text
import Core.Types (Expr(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Bifunctor (Bifunctor(second))
import qualified Data.Text as T

data Token = AddT
           | SubtractT
           | MultiplyT
           | DivideT
           | PowerT
           | NegT
           | OpenParenT
           | CloseParenT
           | PiT
           | ET
           | SinT
           | CosT
           | TanT
           | LogT
           | LnT
           | SqrtT
           | AbsT
           | CommaT
           | VariableT String
           | NumberT Double
           deriving (Show, Eq)

type PrevToken = Maybe Token
type StatefulParser = StateT PrevToken Parser

exprParser :: Parser (Either String Expr)
exprParser = evalStateT exprParserS Nothing

exprParserS :: StatefulParser (Either String Expr)
exprParserS = evalPostfix . opPrecParse <$> tokensParser

tokensParser :: StatefulParser [Token]
tokensParser = tokenParser `sepBy` lift skipSpace

tokenParser :: StatefulParser Token
tokenParser = do
  nextToken <- lift $ choice
    [ string "+" >> return AddT
    , string "-" >> return SubtractT
    , string "*" >> return MultiplyT
    , string "/" >> return DivideT
    , string "^" >> return PowerT
    , string "(" >> return OpenParenT
    , string ")" >> return CloseParenT
    , string "pi" >> return PiT
    , string "e" >> return ET
    , string "sin" >> return SinT
    , string "cos" >> return CosT
    , string "tan" >> return TanT
    , string "log" >> return LogT
    , string "ln" >> return LnT
    , string "sqrt" >> return SqrtT
    , string "abs" >> return AbsT
    , string "," >> return CommaT
    , NumberT <$> double
    , VariableT <$> many1 letter
    ]
  prevToken <- get
  put $ Just nextToken
  return $ case (nextToken, prevToken) of
    (SubtractT, p) -> case ttype <$> p of
      Just OpenParen -> NegT
      Just Comma -> NegT
      Just Operator -> NegT
      Just Function -> NegT
      Nothing -> NegT
      _ -> SubtractT
    _ -> nextToken
      

data TokenType = Operator
              | Function
              | Numlike
              | Comma
              | OpenParen
              | CloseParen
              deriving (Show, Eq)

ttype :: Token -> TokenType
ttype AddT = Operator
ttype SubtractT = Operator
ttype MultiplyT = Operator
ttype DivideT = Operator
ttype PowerT = Operator
ttype NegT = Function
ttype OpenParenT = OpenParen
ttype CloseParenT = CloseParen
ttype PiT = Numlike
ttype ET = Numlike
ttype SinT = Function
ttype CosT = Function
ttype TanT = Function
ttype LogT = Function
ttype LnT = Function
ttype SqrtT = Function
ttype AbsT = Function
ttype CommaT = Comma
ttype (VariableT _) = Numlike
ttype (NumberT _) = Numlike

precedence :: Token -> Int
precedence AddT = 1
precedence SubtractT = 1
precedence MultiplyT = 2
precedence DivideT = 2
precedence PowerT = 3
precedence _ = 0

data Dir = L | R deriving (Show, Eq)

associativity :: Token -> Dir
associativity t | ttype t == Operator = if precedence t == 3 then R else L
                | otherwise = error "Not an operator"

opPrecParse :: [Token] -> Either String [Token]
opPrecParse [] = Left "No input given"
opPrecParse ts =
  second (reverse . uncurry (foldl popOp)) $
  foldl readToken (Right ([], [])) ts

moveFromOpsToOutput :: ([a], [a]) -> ([a], [a])
moveFromOpsToOutput (output', ops') = (head ops':output', tail ops')

isNotLeftParen :: (a, [Token]) -> Bool
isNotLeftParen (_, ops') = (/= OpenParenT) $ head ops'

isNonEmptyandNotLeftParen :: (a, [Token]) -> Bool
isNonEmptyandNotLeftParen (_, ops') = not (null ops') && head ops' /= OpenParenT

isLowerPrecedence :: Token -> (a, [Token]) -> Bool
isLowerPrecedence token (_, op:_) = precedence token < precedence op || precedence token == precedence op && associativity token == L
isLowerPrecedence _ _ = False

popOp :: [Token] -> Token -> [Token]
popOp output' op = case op of
  OpenParenT -> error "Mismatched parentheses"
  _ -> op:output'

readToken :: Either String ([Token], [Token]) -> Token -> Either String ([Token], [Token])
readToken (Left e) _ = Left e
readToken (Right (output, ops)) token = case ttype token of
  Numlike -> Right (token:output, ops)
  Function -> Right (output, token:ops)
  Operator ->
      Right $ Data.Bifunctor.second (token :) $
      iterateWhile ((&&) <$> isNonEmptyandNotLeftParen <*> isLowerPrecedence token) moveFromOpsToOutput (output, ops)
  -- while (
  --      there is an operator o2 at the top of the operator stack which is not a left parenthesis, 
  --      and (o2 has greater precedence than o1 or (o1 and o2 have the same precedence and o1 is left-associative))
  --  ):
  --      pop o2 from the operator stack into the output queue
  --  push o1 onto the operator stack
  Comma ->
    Right $ iterateWhile isNotLeftParen moveFromOpsToOutput (output, ops)
  -- while the operator at the top of the operator stack is not a left parenthesis:
  --       pop the operator from the operator stack into the output queue
  OpenParen -> Right (output, token:ops)
  CloseParen -> case iterateWhile (applyPredicateRight isNotLeftParen) step (Right (output, ops)) of
    (Right (output', OpenParenT:ops')) -> Right $ if not (null ops') && ttype (head ops') == Function
      then (head ops':output', tail ops')
      else (output', ops')
    _ -> Left "Expected open parenthesis"
    where
      applyPredicateRight _ (Left _) = True
      applyPredicateRight p (Right value) = p value
      step (Left e) = Left e
      step (Right (output', ops')) = case ops' of
        [] -> Left "Mismatched parentheses"
        (op:ops'') -> Right (op:output', ops'')
  -- while the operator at the top of the operator stack is not a left parenthesis:
  --      {assert the operator stack is not empty}
  --      /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
  --      pop the operator from the operator stack into the output queue
  --  {assert there is a left parenthesis at the top of the operator stack}
  --  pop the left parenthesis from the operator stack and discard it
  --  if there is a function token at the top of the operator stack, then:
  --      pop the function from the operator stack into the output queue

evalPostfix :: Either String [Token] -> Either String Expr
evalPostfix (Left e) = Left e
evalPostfix (Right []) = Left "No tokens"
evalPostfix (Right (t:ts)) 
  | ttype t == Numlike = Right $ head $ foldl evalTokenOnStack [evalNumlike t] ts
  | otherwise = Left "Invalid expression"

evalNumlike :: Token -> Expr
evalNumlike (NumberT n) = Number n
evalNumlike (VariableT v) = Symbol v
evalNumlike PiT = Pi
evalNumlike ET = E
evalNumlike t | ttype t /= Numlike = error "Not a Numlike token"
              | otherwise = error "Numlike token conversion has not been implemented"

evalTokenOnStack :: [Expr] -> Token -> [Expr]
evalTokenOnStack stack (NumberT n) = Number n:stack
evalTokenOnStack stack (VariableT v) = Symbol v:stack
evalTokenOnStack stack PiT = Pi:stack
evalTokenOnStack stack ET = E:stack
evalTokenOnStack (a:b:stack) AddT = Sum [b,a]:stack
evalTokenOnStack (a:b:stack) SubtractT = Sum [b, -a]:stack
evalTokenOnStack (a:b:stack) MultiplyT = Prod [b, a]:stack
evalTokenOnStack (a:b:stack) DivideT = Prod [b, Pow a $ Number (-1)]:stack
evalTokenOnStack (a:b:stack) PowerT = Pow b a:stack
evalTokenOnStack (a:stack) SinT = Sin a:stack
evalTokenOnStack (a:stack) CosT = Cos a:stack
evalTokenOnStack (a:stack) TanT = Tan a:stack
evalTokenOnStack (a:b:stack) LogT = Log b a:stack
evalTokenOnStack (a:stack) LnT = Ln a:stack
evalTokenOnStack (a:stack) SqrtT = Sqrt a:stack
evalTokenOnStack (a:stack) AbsT = Abs a:stack
evalTokenOnStack (a:stack) NegT = Prod [Number (-1), a]:stack
evalTokenOnStack _ _ = error "Invalid token"

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterateWhile cond step x = if cond x then iterateWhile cond step (step x) else x

runParser :: String -> Either String (Either String Expr)
runParser = parseOnly exprParser . T.pack
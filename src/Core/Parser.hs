module Core.Parser (
  exprParser
) where

import Data.Attoparsec.Text
import qualified Data.Bifunctor
import Core.Types (Expr(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

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

exprParser :: Parser Expr
exprParser = evalStateT exprParserS Nothing

exprParserS :: StatefulParser Expr
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
    , string "Sin" >> return SinT
    , string "Cos" >> return CosT
    , string "Tan" >> return TanT
    , string "Log" >> return LogT
    , string "Ln" >> return LnT
    , string "Sqrt" >> return SqrtT
    , string "Abs" >> return AbsT
    , string "," >> return CommaT
    , NumberT <$> double
    , VariableT <$> many1 letter
    ]
  prevToken <- get
  put $ Just nextToken
  if nextToken /= SubtractT
    then return nextToken
    else case ttype <$> prevToken of
      Just OpenParen -> return NegT
      Just Comma -> return NegT
      Just Operator -> return NegT
      Just Function -> return NegT
      Nothing -> return NegT
      _ -> return SubtractT
      

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

opPrecParse :: [Token] -> [Token]
opPrecParse [] = [] -- TODO: what happens when there are no tokens?
opPrecParse ts =
  reverse $
  uncurry (foldl popOp) $
  foldl readToken ([], []) ts
  where
    moveFromOpsToOutput (output', ops') = (head ops':output', tail ops')
    isNotLeftParen (_, ops') = (/= OpenParenT) $ head ops'
    isNonEmptyandNotLeftParen (_, ops') = not (null ops') && head ops' /= OpenParenT
    isLowerPrecedence token (_, op:_') = precedence token < precedence op || precedence token == precedence op && associativity token == L
    isLowerPrecedence _ _ = False
    popOp output' op = case op of
      OpenParenT -> error "Mismatched parentheses"
      _ -> op:output'
    readToken (output, ops) token = case ttype token of
      Numlike -> (token:output, ops)
      Function -> (output, token:ops)
      Operator -> -- Incomplete - need to handle associativity and adding o1
         Data.Bifunctor.second (token :) $
         iterateWhile ((&&) <$> isNonEmptyandNotLeftParen <*> isLowerPrecedence token)  moveFromOpsToOutput (output, ops)
      -- while (
      --      there is an operator o2 at the top of the operator stack which is not a left parenthesis, 
      --      and (o2 has greater precedence than o1 or (o1 and o2 have the same precedence and o1 is left-associative))
      --  ):
      --      pop o2 from the operator stack into the output queue
      --  push o1 onto the operator stack
      Comma ->
        iterateWhile isNotLeftParen moveFromOpsToOutput (output, ops)
      -- while the operator at the top of the operator stack is not a left parenthesis:
      --       pop the operator from the operator stack into the output queue
      OpenParen -> (output, token:ops)
      CloseParen -> case iterateWhile isNotLeftParen step (output, ops) of
        (output', OpenParenT:ops') -> if not (null ops') && ttype (head ops') == Function
          then (head ops':output', tail ops')
          else (output', ops')
        _ -> error "Expected open parenthesis"
        where
          step (output', ops') = case ops' of
            [] -> error "Mismatched parentheses"
            (op:ops'') -> (op:output', ops'')
      -- while the operator at the top of the operator stack is not a left parenthesis:
      --      {assert the operator stack is not empty}
      --      /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
      --      pop the operator from the operator stack into the output queue
      --  {assert there is a left parenthesis at the top of the operator stack}
      --  pop the left parenthesis from the operator stack and discard it
      --  if there is a function token at the top of the operator stack, then:
      --      pop the function from the operator stack into the output queue

evalPostfix :: [Token] -> Expr
evalPostfix [] = error "No tokens"
evalPostfix (t:ts) | ttype t == Numlike = head $ foldl evalTokenOnStack [evalNumlike t] ts
                   | otherwise = error "Invalid postfix expression"

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
evalTokenOnStack (a:b:stack) AddT = Add b a:stack
evalTokenOnStack (a:b:stack) SubtractT = Sub b a:stack
evalTokenOnStack (a:b:stack) MultiplyT = Mul b a:stack
evalTokenOnStack (a:b:stack) DivideT = Div b a:stack
evalTokenOnStack (a:b:stack) PowerT = Pow b a:stack
evalTokenOnStack (a:stack) SinT = Sin a:stack
evalTokenOnStack (a:stack) CosT = Cos a:stack
evalTokenOnStack (a:stack) TanT = Tan a:stack
evalTokenOnStack (a:b:stack) LogT = Log b a:stack
evalTokenOnStack (a:stack) LnT = Ln a:stack
evalTokenOnStack (a:stack) SqrtT = Sqrt a:stack
evalTokenOnStack (a:stack) AbsT = Abs a:stack
evalTokenOnStack (a:stack) NegT = Neg a:stack
evalTokenOnStack _ _ = error "Invalid token"

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterateWhile cond step x = if cond x then iterateWhile cond step (step x) else x
module Parse (parseLambda, parseLambdaUnsafe) where

import Text.ParserCombinators.Parsec hiding (label)
import Control.Applicative ((<$>))

import Types

parseLambda :: String -> Either ParseError Expression
parseLambda = parse expression ""

parseLambdaUnsafe text = case parseLambda text of
                           Left e  -> error $ show e
                           Right e -> e

expression = variable <|> sExpression

variable = V <$> many1 (letter <|> digit)

sExpression = do
  char '('
  body <- lambda <|> application
  char ')'
  return body

lambda = do
  lambdaSymbol
  whitespace
  param <- variable
  whitespace
  body <- expression
  return $ L param body

lambdaSymbol = char 'λ' <|> char 'L'

application = do
  app <- expression
  whitespace
  arg <- expression
  return $ A app arg

whitespace = many1 (space <|> newline <|> tab)

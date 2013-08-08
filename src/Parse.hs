module Parse (parseLambda) where

import Text.ParserCombinators.Parsec hiding (label)
import Control.Applicative ((<$>))

import Types

parseLambda :: String -> Either ParseError Expression
parseLambda = parse expression ""

expression = variable <|> sExpression

variable = V <$> many1 letter

sExpression = do
  char '('
  body <- lambda <|> application
  char ')'
  return body

lambda = do
  char '#'
  whitespace
  param <- variable
  whitespace
  body <- expression
  return $ L param body

application = do
  app <- expression
  whitespace
  arg <- expression
  return $ A app arg

whitespace = many1 (space <|> newline <|> tab)

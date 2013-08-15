module Parse (parseLambda, parseLambdaUnsafe) where

import Text.ParserCombinators.Parsec hiding (label)
import Control.Applicative ((<$>))

import Types

parseLambda :: String -> Either ParseError Expression
parseLambda = parse expression ""

parseLambdaUnsafe text = case parseLambda text of
                           Left e  -> error $ show e
                           Right e -> e

expression = environment <|> variable <|> sExpression <|> symbolExpression

variable = V <$> many1 (lower <|> digit)

sExpression = do
  char '('
  body <- environment <|> lambda <|> application
  char ')'
  return body

environment = do
  environmentSymbol
  whitespace
  char '['
  bindings <- many1 symbolBinding
  char ']'
  whitespace
  body <- expression
  return $ nestBindings bindings body

environmentSymbol = char '#'

symbolBinding = do
  whitespace
  name <- symbolExpression
  whitespace
  binding <- expression
  whitespace
  return (name, binding)

symbolExpression = S <$> many1 upper

nestBindings [] body                       = body
nestBindings ((symbol, binding):rest) body = E symbol binding $ nestBindings rest body

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

whitespace = many (space <|> newline <|> tab)

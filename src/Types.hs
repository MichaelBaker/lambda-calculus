module Types where

data Expression = V { label :: String }
                | L { param :: Expression, body :: Expression }
                | A { app   :: Expression, arg  :: Expression }
                deriving (Show, Eq)

module Types where

data Expression = V { label  :: String }
                | L { param  :: Expression, body :: Expression }
                | A { app    :: Expression, arg  :: Expression }
                | S { name   :: String }
                | E { symbol :: Expression, value :: Expression, body :: Expression }
                deriving (Show, Eq)

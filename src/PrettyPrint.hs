module PrettyPrint (prettyPrint) where

import Types

prettyPrint (V a)          = a
prettyPrint (L param body) = concat ["(λ ", prettyPrint param, " ", prettyPrint body, ")"]
prettyPrint (A app arg)    = concat ["(", prettyPrint app, " ", prettyPrint arg, ")"]

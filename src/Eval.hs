module Eval (beta) where

import Types

beta (A (L param body) arg) = replace param arg body
beta application            = application

replace (V target) replacement (V body) | target == body = replacement
                                        | otherwise      = V body
replace target replacement (A app arg) = A (replace target replacement app) (replace target replacement arg)
replace target replacement (L param body) | target == param = L param body
                                          | otherwise       = L param (replace target replacement body)

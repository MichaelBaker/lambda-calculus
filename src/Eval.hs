module Eval (alpha, beta, freeVariables) where

import Types

alpha target replacement body@(V _) | target == body = replacement
                                    | otherwise      = body
alpha target replacement (L param body) = L (alpha target replacement param) (alpha target replacement body)
alpha target replacement (A app arg)    = A (alpha target replacement app) (alpha target replacement arg)

beta (A (L param body) arg) = replace param arg body
beta (A (V name) arg)       = A (V name) (beta arg)
beta (A app arg)            = A (beta app) arg
beta application            = application

replace (V target) replacement (V body) | target == body = replacement
                                        | otherwise      = V body
replace target replacement (A app arg) = A (replace target replacement app) (replace target replacement arg)
replace target replacement (L param body) | target == param                            = L param body
                                          | any (== param) (freeVariables replacement) = replace target replacement renamedLambda
                                          | otherwise                                  = L param (replace target replacement body)
                                          where renamedLambda = alpha param newVariable (L param body)
                                                newVariable   = (availableVariable $ freeVariables body ++ freeVariables replacement)

availableVariable takenVariables = head $ filter notTaken $ variableNames 'a' 1
  where notTaken = not . (`elem` takenVariables)
        variableNames 'z'    repetition = variableNames 'a' $ repetition + 1
        variableNames letter repetition = makeVar letter repetition : variableNames (succ letter) repetition
        makeVar letter repetition = V $ take repetition $ repeat letter

freeVariables (V a)          = [V a]
freeVariables (L param body) = filter (/= param) $ freeVariables body
freeVariables (A app arg)    = freeVariables app ++ freeVariables arg

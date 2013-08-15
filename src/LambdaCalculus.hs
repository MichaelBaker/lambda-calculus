import System.Environment (getArgs)
import Data.List          (intercalate)
import Safe               (readMay)
import Parse              (parseLambda)
import PrettyPrint        (prettyPrint)
import Eval               (beta)

data Mode = ReduceOnce
          | FullReduction
          | ReduceNTimes Integer
          deriving (Show, Read, Eq)

allModes = [ReduceOnce, FullReduction, ReduceNTimes 10]

main = do
  mode <- getMode
  text <- getContents

  case parseLambda text of
    Left e  -> error $ show e
    Right e -> mapM_ (putStrLn . formatRow) $ zip [0..] (e : reduce e mode)

formatRow (number, expression) = concat [show number, ".\t", prettyPrint expression]

getMode = do
  args <- getArgs
  if null args
    then return FullReduction
    else do
      case readMay $ intercalate " " args of
        Nothing   -> error $ "Invalid reduction mode '" ++ head args ++ "'\n\nValid modes are:\n" ++ intercalate "\n" (map show allModes)
        Just mode -> return mode

reduce expression ReduceOnce    = [beta expression]
reduce expression FullReduction | fullyReduced = [expression]
                                | otherwise    = reduction : reduce reduction FullReduction
                                where reduction    = beta expression
                                      fullyReduced = expression == reduction
reduce expression (ReduceNTimes 0) = [expression]
reduce expression (ReduceNTimes 1) = [expression]
reduce expression (ReduceNTimes n) = beta expression : reduce (beta expression) (ReduceNTimes $ n -1)

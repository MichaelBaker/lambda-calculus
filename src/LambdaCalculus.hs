import System.Environment (getArgs)
import Data.List          (intercalate)
import Safe               (readMay)
import Parse              (parseLambda)
import PrettyPrint        (prettyPrint)
import Eval               (beta)

data Mode = ReduceOnce
          | FullReduction
          deriving (Show, Read, Eq)

allModes = [ReduceOnce, FullReduction]

main = do
  mode <- getMode
  text <- getContents

  case parseLambda text of
    Left e  -> error $ show e
    Right e -> putStrLn $ prettyPrint $ last $ reduce e mode

getMode = do
  args <- getArgs
  if null args
    then return FullReduction
    else do
      case readMay $ head args of
        Nothing   -> error $ "Invalid reduction mode '" ++ head args ++ "'\n\nValid modes are:\n" ++ intercalate "\n" (map show allModes)
        Just mode -> return mode

reduce expression ReduceOnce    = [beta expression]
reduce expression FullReduction | fullyReduced = [expression]
                                | otherwise    = reduction : reduce reduction FullReduction
                                where reduction    = beta expression
                                      fullyReduced = expression == reduction

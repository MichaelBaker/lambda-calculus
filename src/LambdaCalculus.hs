import System.Environment (getArgs)
import Data.List          (intercalate)
import Safe               (readMay)
import Parse              (parseLambda)
import PrettyPrint        (prettyPrint)
import Eval               (beta)

data Mode = Once
          | Full
          | Ntimes Integer
          deriving (Show, Read, Eq)

allModes = [Once, Full, Ntimes 10]

main = do
  mode <- getMode
  text <- getContents

  case parseLambda text of
    Left e  -> error $ show e
    Right e -> mapM_ (putStrLn . formatRow) $ zip [0..] ((e, []) : reduce (e, []) mode)

formatRow (number, (expression, _)) = concat [show number, ".\t", prettyPrint expression]

getMode = do
  args <- getArgs
  if null args
    then return Full
    else case readMay $ intercalate " " args of
           Nothing   -> error $ "Invalid reduction mode '" ++ head args ++ "'\n\nValid modes are:\n" ++ intercalate "\n" (map show allModes)
           Just mode -> return mode

reduce expression Once    = reduce expression (Ntimes 1)
reduce expression Full | fullyReduced = []
                       | otherwise    = reduction : reduce reduction Full
                       where reduction    = beta expression
                             fullyReduced = fst expression == fst reduction
reduce expression (Ntimes 0) = []
reduce expression (Ntimes n) | fullyReduced = []
                             | otherwise    = reduction : reduce reduction (Ntimes $ n -1)
                             where reduction    = beta expression
                                   fullyReduced = fst expression == fst reduction

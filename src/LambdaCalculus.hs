import Parse       (parseLambda)
import PrettyPrint (prettyPrint)
import Eval        (beta)

main = do
  text <- getContents
  case parseLambda text of
    Left e  -> error $ show e
    Right e -> putStrLn $ prettyPrint $ beta e

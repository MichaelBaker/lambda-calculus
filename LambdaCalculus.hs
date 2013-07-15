data Expression = Variable    { label :: String }
                | Lambda      { parameter :: Expression, body :: Expression }
                | Application { applied :: Expression, argument :: Expression }
                deriving (Show)

prettyPrint :: Expression -> String
prettyPrint (Variable a)                   = a
prettyPrint (Lambda parameter body)        = concat ["(", "λ ", prettyPrint parameter, ".", prettyPrint body, ")"]
prettyPrint (Application applied argument) = prettyPrint applied ++ prettyPrint argument

main :: IO ()
main = do
  putStrLn $ prettyPrint $ Variable "a"
  putStrLn $ prettyPrint $ Lambda (Variable "a") (Variable "a")
  putStrLn $ prettyPrint $ Application (Lambda (Variable "a") (Application (Variable "a") (Variable "a"))) (Lambda (Variable "b") (Variable "b"))
  putStrLn $ prettyPrint $ Lambda (Variable "a") (Application (Lambda (Variable "b") (Variable "b")) (Lambda (Variable "b") (Variable "b")))
  putStrLn $ prettyPrint $ Lambda (Variable "a") (Lambda (Variable "b") (Lambda (Variable "b") (Variable "b")))

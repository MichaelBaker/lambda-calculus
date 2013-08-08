import Parse (parseLambda)

main = do
  text <- getContents
  print $ parseLambda $ text

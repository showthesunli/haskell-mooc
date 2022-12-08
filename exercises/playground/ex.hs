ask [] = return []
ask (question : questions) = do
  putStr question
  putStrLn "?"
  answer <- getLine
  answers <- ask questions
  return (answer : answers)
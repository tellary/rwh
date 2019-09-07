main = putStrLn "Greetings! What is your name?" >>
  getLine >>=
  (\inpStr ->
      putStrLn $ "Welcome, " ++ inpStr ++ "!")

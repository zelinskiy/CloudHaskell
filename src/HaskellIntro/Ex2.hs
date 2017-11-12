import Prelude

main :: IO ()
main = do
  putStrLn "Insert your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

main2 = putStrLn "Insert your name:"
  >> getLine
  >>= \name ->
        putStrLn ("Hello, " ++ name ++ "!")

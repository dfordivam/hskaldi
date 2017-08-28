module Main where


foreign import ccall unsafe "c_main"
               c_main :: IO ()

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  c_main

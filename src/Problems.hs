module Problems where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

problem1 a = head $ reverse a

problem2 a =  last $ take 2 $ reverse a
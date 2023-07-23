module Problems where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

problem1 a = head $ reverse a

problem2 a = last $ take 2 $ reverse a

problem3 a 1 = head a
problem3 (_ : as) n = problem3 as (n - 1)

problem4 a = length a

problem5 a = reverse a

problem6 a = a == reverse a
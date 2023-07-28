module Problems where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- the goal here is to do an entire section of problems,
-- then review alternative solutions

problem1 a = head $ reverse a

problem2 a = last $ take 2 $ reverse a

problem3 a 1 = head a
problem3 (_ : as) n = problem3 as (n - 1)

problem4 a = length a

problem5 a = reverse a

problem6 a = a == reverse a

data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)

problem7 :: Num a => NestedList a -> [a]
problem7 (Elem a   )   = [a]
problem7 (List (x:xs)) = problem7 x ++ problem7 (List xs)
problem7 (List [])     = []
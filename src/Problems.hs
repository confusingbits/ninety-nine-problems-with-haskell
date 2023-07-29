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

-- why does this require a constraint or cast at the caller? (Num a =>)
problem7 :: NestedList a -> [a]
problem7 (Elem a   )   = [a]
problem7 (List (x:xs)) = problem7 x ++ problem7 (List xs) -- i don't understand the List xs part
problem7 (List [])     = []
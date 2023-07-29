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
problem7 (Elem a) = [a]
problem7 (List (x : xs)) = problem7 x ++ problem7 (List xs) -- i don't understand the List xs part
problem7 (List []) = []

dedup :: Char -> String -> String
dedup a "" = [a]
dedup a acc =
  let l = last acc
   in if l == a
        then acc
        else acc ++ [a]

problem8 :: String -> String
problem8 a = reverse $ foldr dedup "" a

combine :: Char -> [String] -> [String]
combine c [] = [[c]]
combine c acc =
  let
    -- [['a'], ['b']]
    r = reverse acc -- [['b'], ['a']]
    x = head r -- ['b']
    xs = tail r -- [['a']]
    contained = elem c x
    -- if the last array of the list contains the char
   in if contained
        -- append the last array
        then reverse $ (x ++ [c]) : xs
        -- else create a new array with the char
        else reverse $ [c] : r -- ['a', 'b', 'c']

problem9 :: String -> [String]
problem9 a = reverse $ foldr combine [] a

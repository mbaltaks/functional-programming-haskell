-- https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/120258
-- Algebraic data types


-- alternatives, or "sum" data types
data SimpleNumber = One | Two | Many deriving Show
convert 1 = One
convert 2 = Two
convert _ = Many

-- map convert [1..6]


-- record or product data types
data CricketScore = Score [Char] Int Int deriving Show

-- let x = Score "NZ" 167 6

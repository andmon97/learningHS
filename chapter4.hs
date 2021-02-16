-- CONDITIONAL EXPRESSION --
-- valore assoluto --
abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

-- Nested if then else --
signum'  :: Int -> Int
signum' n = if n < 0 then -1 else
              if n == 0 then 0 else 1



-- GUARDED EQUATIONS --
-- We can define the same conditional expressions using the guarded expressions--
absGuarded n    | n>= 0     = n
                | otherwise = -n 

signumGuarded n | n < 0     = -1
                | n == 0    = 0
                | otherwise = 1



-- PATTERN MATCHING --
-- Many functions have a particularly clear definition using pattern matching on their arguments --

-- not maps False to True, and True to False --
not' :: Bool -> Bool
not' False = True
not' True = False

-- Functions can often be defined in many different ways using pattern matching. For the && we can map all the truth table or more compactly: --
(&&&)            :: Bool -> Bool -> Bool
True &&& True    = True
_ &&& _          = False
-- The underscore symbol _ is a wildcard pattern that matches any argument value--
-- However, the following definition is more efficient, because it avoids evaluating the second argument if the first argument is False --
True &&& b    = b 
False &&& _   = False 




-- LIST PATTERNS --
--[1,2,3,4] Means 1:(2:(3:(4:[]))) operator (:) adds an element to the start of a list--

-- head and tail map any non-empty list to its first and remaining elements -- 
head'            :: [a] -> a 
head' (x : _)    = x 

tail'            :: [a] -> [a]
tail' (_ : xs)   = xs
--NB x:xs patterns must be parenthesised, because application has priority over (:) --



-- INTEGER PATTERNS --

{-pred        :: Int -> Int
pred (n+1)  = n 
-}
-- Pred maps any positive integer to its predecessor. Patterns must be parenthesised, because app has priority over. WORKS IN HS 98 NOT IN 2010!!!--



-- LAMBDA EXPRESSION
--Lambda expressions can be used to give a formal meaning to functions defined using currying

{-
odds :: Int → [Int ]
odds n = map f [0 . . n − 1]
where f x = x ∗ 2 + 1
-}
-- can be
odds n = map (\x -> x * 2 + 1) [0 .. n-1]


-- DEPENDANT GENERATORS
-- Using a dependant generator we can define the library function that concatenates a list of lists:

concat'      :: [[a]] -> [a]
concat' xss  = [x | xs <- xss, x <- xs]
{- example
> concat [[1,2,3],[4,5],[6]]
[1,2,3,4,5,6]
-}



--              EXCERCISES              ---
-- "Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail using:
-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) patternmatching"
safetailA       :: [a] -> [a]
safetailA xs    = if null xs then [] else tail xs

safetailB       :: [a] -> [a]
safetailB xs
            | null xs = []
            | otherwise = tail xs

safetailC       :: [a] -> [a]
safetailC []    = []
safetailC (_:xs)= tail xs

-- "Give three possible definitions for the logical or operator (||) using pattern matching"


-- "Redefine the following version of (&&) using conditionals rather than patterns
-- True && True = True
-- _ &&_ =False"
(&&&&)           :: Bool -> Bool -> Bool
(&&&&) a b       = if a == True && b == True then True else False

-- "Do the same for the following version:
-- True && b = b
-- False && _ = False"
(&&&&&)           :: Bool -> Bool -> Bool
(&&&&&) a b       = if a == False then False else b

-- FIRST FUNCTIONS--
-- Raddoppia x param e quadrupla richiamando double di double di param x --
double x = x + x
quadruple x = double (double x)

-- Fattoriale di un numero come prodotto di elementi di una lista contentente da uno a n--
factorial n = product [1..n]

-- n é uguale a 10 diviso 2 cioé lunghezza di xs--
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]



-- verifica tipo di queste funzioni--
-- comando :type--
second xs     = Prelude.head (Prelude.tail xs)

swap (x,y)    = (y,x)

pair x y      = (x,y)

palindrome xs = reverse xs == xs

twice f x     = f (f x) 



-- CONDITIONAL EXPRESSION --
-- valore assoluto --
abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- Nested if then else --
signum  :: Int -> Int
signum n = if n < 0 then -1 else
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
not :: Bool -> Bool
not False = True
not True = False

-- Functions can often be defined in many different ways using pattern matching. For the && we can map all the truth table or more compactly: --
(&&)            :: Bool -> Bool -> Bool
True && True    = True
_ && _          = False
-- The underscore symbol _ is a wildcard pattern that matches any argument value--
-- However, the following definition is more efficient, because it avoids evaluating the second argument if the first argument is False --
True && b    = b 
False && _   = False 



-- LIST PATTERNS --
--[1,2,3,4] Means 1:(2:(3:(4:[]))) operator (:) adds an element to the start of a list--

-- head and tail map any non-empty list to its first and remaining elements -- 
head            :: [a] -> a 
head (x : _)    = x 

tail            :: [a] -> [a]
tail (_ : xs)   = xs
--NB x:xs patterns must be parenthesised, because application has priority over (:) --



-- INTEGER PATTERNS --

pred        :: Int -> Int
pred (n+1)  = n 
-- Pred maps any positive integer to its predecessor. Patterns must be parenthesised, because app has priority over + --
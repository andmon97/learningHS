import Data.Char


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

concat      :: [[a]] -> [a]
concat xss  = [x | xs <- xss, x <- xs]
{- example
> concat [[1,2,3],[4,5],[6]]
[1,2,3,4,5,6]
-}



-- GUARDS 
-- List comprehensions can use guards to restrict the values produced by earlier generators

-- Using a guard we can define a function that maps a positive integer to its list of factors:
factors     :: Int -> [Int]
factors n   = 
    [x | x <- [1..n], n `mod` x == 0]

-- A positive integer is prime if its only factors are 1 and itself.  Hence, using factors we can define a function that decides if a number is prime:
prime       :: Int -> Bool
prime n     = factors n == [1,n]

-- Using a guard we can now define a function that returns the list of all primes up to a given limit:
primes      :: Int -> [Int]
primes n    = 
    [x | x <- [1..n], factors x == [1,x]]  -- ALSO primes n = [x | x  [2..n], prime x] but it works :)

{-
The Zip Function
A useful library function is zip, which maps two lists to a list of pairs of their corresponding elements.
zip :: [a] -> [b] -> [(a,b)]

ex
> zip [’a’,’b’,’c’] [1,2,3,4]

[(’a’,1),(’b’,2),(’c’,3)]
-}

-- Using zip we can define a function returns the list of all pairs of adjacent elements from a list:
adjacents       :: [a] -> [(a,a)]
adjacents xs    = zip xs (Main.tail xs)

--Using adjacents we can define a function that decides if the elements in a list are sorted:
sorted          :: Ord a => [a] -> Bool
sorted xs       =
    and [x <= y | (x,y) <- adjacents xs]




-- STRINGS COMPREHENSION
--A string is a sequence of characters enclosed in double quotes.  Internally, however, strings are represented as lists of characters.

--Similarly, list comprehensions can also be used to define functions on strings, such as a function that counts the lower-case letters in a string
lowers   :: String -> Int
lowers xs =
   length [x | x <- xs, Data.Char.isLower x]


-- EXCERCISES
-- A triple (x,y,z) of positive integers is called pythagorean if x^2 + y^2 = z^2.  Using a list comprehension, define a function that maps an integer n to all such triples with components in [1..n].  

pyths           :: Int -> [(Int, Int, Int)]
pyths n         =
    [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

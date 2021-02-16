import Data.Char


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

-- A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.  Using a list comprehension, define a function that returns the list of all perfect numbers up to a given limit
factors'        :: Int -> [Int]
factors' n      =
    [x | x <- [1..n-1], n `mod` x == 0]

perfects        :: Int -> [Int]
perfects n      = 
    [x | x <- [1..n], sum (factors' x) == x]

-- "The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers. Using a list comprehension, define a function that returns the scalar product of two lists"
scalar_product          :: [Int] -> [Int] -> Int 
scalar_product xs ys    =
    sum [x * y | (x,y) <- zip xs ys]

--      HIGHER ORDER FUNCTIONS      --  
-- A function is called higher-order if it takes a function as an argument or returns a function as a result. For ex twice is the double application of a function to a var x --

twice'          :: (a -> a) -> a -> a 
twice' f x      = f (f x)

-- The higher-order library function called map applies a function to every element of a list. Other definitions here--
map'            :: (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x:xs)   = f x : map' f xs

map''           :: (a -> b) -> [a] -> [b]
map'' f xs      = [f x | x <- xs]

-- The higher-order library function filter selects every element from a list that satisfies a predicate. Some definitions here --
filter'         :: (a -> Bool) -> [a] -> [a]
filter' p xs    = [x | x <- xs, p x]

filter''        :: (a -> Bool) -> [a] -> [a]
filter'' p []   = []
filter'' p (x:xs)   
    | p x       = x : filter'' p xs 
    | otherwise = filter'' p xs



--      FOLDR PATTERN FUNCTION      --  
--examples of functions

{- 
sum []  = 0
sum (x:xs) = x + sum xs

sum     = foldr (+) 0
----------------------------------------
product []     = 1
product (x:xs) = x * product xs

product = foldr (*) 1
----------------------------------------

and []     = True
and (x:xs) = x && and xs

and     = foldr (&&) True

FOLDR associate to right
-}

length'         :: [a] -> Int
length'         = foldr (\_ n -> 1+n) 0

reverse'        :: [a] -> [a]
reverse'        = foldr (\x xs -> xs ++ [x]) [] 



--          COMPOSITION FUNCTION        --
-- The library function (.) returns the composition of two functions as a single function. (...) is a re-definition of (.) --
(...)            :: (b ->c) -> (a -> b) -> (a -> c)
f ... g          = \x -> f (g x)

-- Example of applications
odd'            :: Int -> Bool
odd'            = not ... even



-- Functions with a predicate --
-- The library function all decides if every element of a list satisfies a given predicate.all' is a re-definition --
all'            :: (a -> Bool) -> [a] -> Bool
all' p xs       = and [p x | x <- xs]

-- Dually, the library function any decides if at least one element of a list satisfies a predicate. any' is a re- definition -- 
any'            :: (a -> Bool) -> [a] -> Bool
any' p xs       = or [p x | x <- xs]

-- The library function takeWhile selects elements from a list while a predicate holds of all the elements. --
takeWhile'      :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
        | p x       = x : takeWhile' p xs
        | otherwise = [] 



-- EXERCISES --

-- "Express the comprehension [f x | x ← xs, p x] using the functions map and filter."
comprehension xs f p = map f (filter p xs) -- can also be comprehension xs f p = map f $ filter p xs

-- "Redefine map f and filter p using foldr."
map''' f xs     = foldr (\x function -> f x: function) []

filter''' p xs  = foldr (\x predicate -> if p x then x : predicate  else predicate) []
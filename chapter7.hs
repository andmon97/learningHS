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



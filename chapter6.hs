{-# LANGUAGE NPlusKPatterns #-}

-- CHAPTER 6 RECURSIVE FUNCTIONS
-- In Haskell, functions can also be defined in terms of themselves.  Such functions are called recursive.
factorial 0         = 1
factorial (n + 1)   = (n + 1) * factorial n



-- Recursion on Lists

-- product maps the empty list to 1, and any non-empty list to its head multiplied by the product of its tail. (call productR to avoid ambiguous occurrence)
productR         :: [Int] -> Int
productR []      = 1
productR (n:ns)  = n * productR ns

-- Using the same pattern of recursion as in product we can define the length function on lists.(call lengthR to avoid ambiguous occurrence)
lengthR          :: [a] -> Int 
lengthR []       = 0
lengthR (_ : xs) = 1 + lengthR xs


-- Using a similar pattern of recursion we can define the reverse function on lists. (call reverseR to avoid ambiguous occurrence)
reverseR         :: [a] -> [a]
reverseR []      = []
reverseR (x:xs)  = reverseR xs ++ [x]



-- MULTIPLE ARGUMENTS
-- Functions with more than one argument can also be defined using recursion.  (call zipR to avoid ambiguous occurrence)

-- Zipping the elements of two lists
zipR                :: [a] -> [b] -> [(a,b)]
zipR [] _           = []
zipR _ []           = []
zipR (x:xs) (y:ys)  = (x,y) : zipR xs ys
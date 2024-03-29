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

-- Remove the first n elements from a list
dropR                :: Int -> [a] -> [a]
dropR 0     xs       = xs
dropR (n+1) []       = []
dropR (n+1) (_:xs)   = dropR n xs



--     QUICK SORT       --
{-
The quicksort algorithm for sorting a list of integers can be specified by the following two rules:
1) The empty list is already sorted;
2) Non-empty lists can be sorted by sorting the tail values <= the head, sorting the tail values > the head, and then appending the resulting lists on either side of the head value.
-}
qSort           :: [Int] -> [Int]
qSort []        = []
qSort (x:xs)    =
    qSort smaller ++ [x] ++ qSort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]



--      EXCERCISES      --

-- "Decide if all logical values in a list are true" -- 
and'                :: [Bool] -> Bool
and' []             = True 
and' (false : _)    = False
and' (_ : xs)       = and' xs

-- "Concatenate a list of lists"
concat'             :: [[a]] -> [a]
concat' []          = []
concat' (x:xs)      = x ++ concat' xs

-- "Produce a list with n identical elements"
replicate'          :: Int -> a -> [a]
replicate' 0 x      = []
replicate' n x      = x : replicate' (n-1) x

-- "Select the nth element of a list"
(!!!)               :: [a] -> Int -> a
(x : _)  !!! 0      = x
(_ : xs) !!! n      = xs !!! (n-1)

-- "Decide if a value is an element of a list"
elem'               :: Eq a => a -> [a] -> Bool
elem' y []          = False
elem' y (x:xs)      = if y == x then True else elem' y xs

-- "Define a recursive function merge ∷ Ord a ⇒ [a] → [a] → [a] that merges two sorted lists of values to give a single sorted list"
merge               :: Ord a => [a] -> [a] -> [a]
merge [] []         = []
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) =
    if x < y 
        then x : merge xs (y:ys)
        else y : merge (x:xs) ys


-- "Define a recursive function msort ∷ Orda ⇒ [a] → [a] that implements merge sort, which can be specified by the following two rules:
-- 1) Lists of length ≤ 1 are already sorted;
-- 2) Other lists can be sorted by sorting the two halves and merging the resulting lists"
msort               :: Ord a => [a] -> [a]
msort []            = []
msort (x : [])      = [x]
msort xs            =
    merge (msort (take divideAt xs)) (msort (drop divideAt xs))
    where
        divideAt = div (length xs) 2
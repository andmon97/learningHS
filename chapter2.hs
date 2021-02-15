-- FIRST FUNCTIONS--

-- Duble and quadruple an x--
double x = x + x
quadruple x = double (double x)

-- factorial of a number n--
factorial n = product [1..n]



--- EXCERCISES ---

-- "Show how the library function last that selects the last element of a list can be defined using the funcions introduce in this lecture"
-- The mentioned functions are head, tail, !!, take, drop, length, sum, product, ++, reverse and div

last'       :: [a] -> a
last' xs     = head (reverse xs)

-- "Can you think of another possible definition?"

last''      :: [a] -> a
last'' xs   = xs !! (length xs - 1)

-- "Similarly, show how the library function init that removes the last element from a list can be defined in two different ways"
init'       :: [a] -> [a]
init'  xs   = take (length xs - 1) xs

init''      :: [a] -> [a]
init''  xs  = reverse (tail (reverse xs))
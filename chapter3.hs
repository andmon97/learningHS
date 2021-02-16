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




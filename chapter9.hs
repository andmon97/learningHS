{-# LANGUAGE BlockArguments #-}
--          SEQUENCING
-- A sequence of actions can be combined as a single composite action using the keyword do
a :: IO (Char , Char )
a = do
        x <- getChar 
        getChar 
        y <- getChar 
        return (x, y)

--          DERIVED PRIMES
-- reading a string from the keyboard (getLine already exists in prelude)
getLine'    :: IO String 
getLine'    = do 
                x <- getChar 
                if x == '\n' then
                    return []
                else
                    do 
                    xs <- getLine' 
                    return (x:xs)

{- Dually, we define actions putStr and putStrLn that write a string to the screen, with the latter action also moving on to a new line afterwards -}
putStr'         :: String -> IO ()
putStr' []      = return  ()
putStr' (x:xs)  = do putChar  x
                     putStr' xs 
    
-- Writing a string and moving to a new line
putStrLn'       :: String -> IO ()
putStrLn' xs    = do putStr' xs
                     putChar '\n'

-- EXAMPLE: read a string and output its length 
strlen          :: IO ()
strlen          = do
                        putStr' "Enter a string: "
                        xs <- getLine'
                        putStr' "The string has "
                        putStr' (show (length xs))
                        putStrLn' " characters"
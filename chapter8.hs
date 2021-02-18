--                                                  PARSERS 
-- definition of the type parser 
type Parser a   = String -> [(a, String)]

--          BASIC PARSERS           --

-- the parser return v always succeeds with the result value v, without consuming any of the input string
return          :: a -> Parser a 
return v        = \inp -> [(v, inp)]

-- Whereas return v always succeeds, the dual parser failure always fails, regardless of the contents of the input string
failure         :: Parser a 
failure         = \inp -> []

-- Our final basic parser is item, which fails if the input string is empty, and succeeds with the first character as the result value otherwise
item            :: Parser Char 
item            = \inp -> case inp of 
                            [] -> []
                            (x:xs) -> [(x, xs)]

{- 
The case mechanism of Haskell used in this definition allows pattern matching to be used in the body of a definition, in this example by matching the string
inp against two patterns to choose between two possible results. The case mechanism is not used much in this book, but can sometimes be useful.
Because parsers are functions, they could be applied to a string using normal function application, butwe prefer to abstract from the representation of parsers
by defining our own application function 
-}
parse           :: Parser a -> String -> [(a, String)]
parse p inp     = p inp



--          SEQUENCING          --
-- In practice, however, it turns out to be more convenient to combine the sequencing of parsers with the processing of their result values, by means of a sequencing operator >>= (read as “then”) defined as follows--
(>>=)           :: Parser a -> (a -> Parser b) -> Parser b 
p >>= f         = \inp -> case parse p inp of 
                            [] -> []
                            [(v, out)] -> parse (f v) out

{-  p >>= f fails if the application of the parser p to the input
string fails, and otherwise applies the function f to the result value to give a
second parser, which is then applied to the output string to give the final result.-}

-- DO SYNTAX --
-- the do notation for sequencing parsers, in the sense that each parser in the sequence must begin in precisely the same column
-- examplea parser that consumes three characters, discards the second, and returns the first and third as a pair can now be defined as follows



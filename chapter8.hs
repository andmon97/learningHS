--                                                  PARSERS 

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
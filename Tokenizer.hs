module Tokenizer (
    Token(..),
    TokenType(..),
    tokenize,
    removeEmpty
    ) where
import System.IO
import Data.Char
import Data.List as L





-- this file is taken from the underscore c compiler source code. 


--remove space tokens, but not line breaks.
removeEmpty :: [Token] -> [Token]
removeEmpty xs = filter isTokenQualified xs
                 where isTokenQualified tk = let char =  head $ getTokenContent tk
                                             in ((not $ isSpace char) || (char=='\n'))



data Token = Token 
                    { getTokenContent :: String -- string content
                    , getTokenType :: TokenType  -- Identifier if the token is valid ID([a-zA-Z0-9], 
                    } deriving (Show, Eq)

data TokenType = Identifier | Comment | StringLiteral | LineBreak | SpaceCharacter | SpecialCharacter
                    deriving (Show, Eq)




-- Converts a string into tokens, [A-Za-z0-9_] are grouped, 
-- C Style comments are grouped. # is treated the same way as //
-- Everything else is character seperated, including spaces and tabs.
-- Use removeEmpty to remove spaces and tabs of the generated token.
tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isValidID x = Token (x:(takeWhile isValidID xs)) Identifier: tokenize (dropWhile isValidID xs)
    | x == '\n' = Token "\n" LineBreak : tokenize xs
    | isSpace x = Token (x:[]) SpaceCharacter: tokenize xs
    | isQuote x = Token (x:(fst $ quoteEscape xs x)) StringLiteral : tokenize ( snd $ quoteEscape xs x)
    -- ignore # until end of sentence, using quote escape
    | x == '#' = Token (x:(fst $ quoteEscape xs '\n')) Comment : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && xs == [] = []
    | x == '/' && head xs == '/' = Token (x:(fst $ quoteEscape xs '\n')) Comment : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && head xs == '*' = Token ('/':'*':(fst $ multiLineCommentEscape xs)) Comment 
                                                    : tokenize (snd $ multiLineCommentEscape xs)
    | otherwise = Token [x] SpecialCharacter: tokenize xs

-- whether the char is a valid id
isValidID :: Char -> Bool
isValidID x = (isAlphaNum x ) || ( x == '_')

-- whether the car is a single or a double quote
isQuote :: Char -> Bool
isQuote x = x == '\"' || x == '\''

-- Escape sequence until */ is encountered
multiLineCommentEscape :: String -> (String, String)
multiLineCommentEscape str = multiLineCommentEscapeRec ("", str)
        where multiLineCommentEscapeRec :: (String, String) -> (String, String)
              multiLineCommentEscapeRec (a, []) = (a, []) -- should not happen
              multiLineCommentEscapeRec (a, ('*':'/':xs)) = (a ++ "*/", xs)
              multiLineCommentEscapeRec (a, (x:xs)) = multiLineCommentEscapeRec (a ++ [x], xs)


--quote escape :: hel, fjis\"jisjv"sub -> " -> (hel, fjis\"jisjv", sub)
--quote escape can be used to escape other sequences of character, e.g. \n will split at \n
quoteEscape :: String -> Char -> (String, String) -- string -> " or ' -> inquote, afterquote
quoteEscape str q = quoteEscapeRec ("", str) 
                  where quoteEscapeRec :: (String, String) -> (String, String)
                        --single escape
                        quoteEscapeRec (a,('\\':x:xs)) = quoteEscapeRec (a ++ ['\\'] ++ [x], xs)
                        quoteEscapeRec (a,(x:xs)) = if x == q then (a++[q], xs) else quoteEscapeRec (a++[x], xs)
                        quoteEscapeRec (a, [] ) = (a, []) -- should not happen


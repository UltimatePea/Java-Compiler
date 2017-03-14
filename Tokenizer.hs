module Tokenizer (
    Token(..),
    getTokenContent, 
    isTokenValidID, 
    tokenize
    ) where
import System.IO
import Data.Char
import Data.List as L





-- this file is taken from the underscore c compiler source code. 



main = putStrLn "Hello"



data Token = Token 
                    String -- string content
                    Bool  -- True if the token is valid ID([a-zA-Z0-9], false otherwise
                    deriving (Show, Ord, Eq)

-- parses and gets the token content that is compatible with C Macro
getTokenContent :: Token -> String
getTokenContent (Token s _) = s
isTokenValidID :: Token -> Bool
isTokenValidID (Token _ b) = b



tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isValidID x = Token (x:(takeWhile isValidID xs)) True: tokenize (dropWhile isValidID xs)
    | x == '\n' = Token "\n" False : tokenize xs
    | isSpace x = Token (x:[]) False: tokenize xs
    | isQuote x = Token (x:(fst $ quoteEscape xs x)) False : tokenize ( snd $ quoteEscape xs x)
    -- ignore # until end of sentence, using quote escape
    | x == '#' = Token (x:(fst $ quoteEscape xs '\n')) False : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && xs == [] = []
    | x == '/' && head xs == '/' = Token (x:(fst $ quoteEscape xs '\n')) False : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && head xs == '*' = Token ('/':'*':(fst $ multiLineCommentEscape xs)) False 
                                                    : tokenize (snd $ multiLineCommentEscape xs)
    | otherwise = Token [x] False: tokenize xs

isValidID :: Char -> Bool
isValidID x = (isAlphaNum x ) || ( x == '_')

isQuote :: Char -> Bool
isQuote x = x == '\"' || x == '\''

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


module ParenthesisTree (
    ParenthesisTree(..),
    parseParenthesisTree
) where
data ParenthesisTree = Empty
                       | Plain String 
                       | Tree ParenthesisTree ParenthesisTree
                        deriving (Show)



data ParserState = Wait 
                 | InParen ParserState -- inside a small parenthesis, parent state
                 | InBracket ParserState -- parent state
                 | InBrace ParserState -- parent state
                 
parseParenthesisTree :: String  -- String remaining to digest
                        -> ParserState -- the state of the parser
                        -> ParenthesisTree -- returns a parenthesis tree

parseParenthesisTree [] state = case state of 
                                     Wait -> Empty
                                     InParen _ -> error "Error: Expected ')' at the end"
                                     InBracket _ -> error "Error: Expected ']' at the end"
                                     InBrace _ -> error "Error: Expected ']' at the end"
parseParenthesisTree ('(':xs) state = parseParenthesisTree xs (InParen state)
parseParenthesisTree ('[':xs) state = parseParenthesisTree xs (InBracket state)
parseParenthesisTree ('{':xs) state = parseParenthesisTree xs (InBrace state)
parseParenthesisTree (')':xs) state = case state of 
                                           (InParen parent) -> parseParenthesisTree xs parent
                                           _ -> error "Unexpected `)`"
parseParenthesisTree (']':xs) state = case state of 
                                           (InBracket parent) -> parseParenthesisTree xs parent
                                           _ -> error "Unexpected `]`"
parseParenthesisTree ('}':xs) state = case state of 
                                           (InBrace parent) -> parseParenthesisTree xs parent
                                           _ -> error "Unexpected `]`"

parseParenthesisTree str state = let isNotSeperator c = not $ (c `elem` "()[]{}")
                                 in Tree (Plain (takeWhile isNotSeperator str)) $
                                        parseParenthesisTree (dropWhile isNotSeperator str) state

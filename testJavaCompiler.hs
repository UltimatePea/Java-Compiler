data SyntaxTree = Empty
                 |If {
                       condition :: SyntaxTree
                     , ifbody :: SyntaxTree
                     , elsebody :: SyntaxTree
                 }
                 |Assign VariableName SyntaxTree -- a = ..
                 deriving (Show)


data VariableName = VariableName String deriving (Show)
data Sequence = Sequence SyntaxTree Sequence
               



fromString :: String -> Maybe SyntaxTree
fromString [] = return Empty
fromString xs
    | head xs == ";" = return Empty
    | take 2 xs == "if" = return Empty
                   

import Data.Char
import System.IO;
import Tokenizer;
import Debug.Trace;
import qualified Data.List as L
import qualified Data.List.Split as LSplit



main = do  inputStr <- hGetContents stdin
           putStrLn $ process inputStr



process :: String -> String
process str = L.intercalate "\n" $ -- pretty print
                    map show $ -- convert declarations to string
                    interpreteTopLevel $  -- get a list of tokens 
                    filter (\x -> not $isSpace $ head $ getTokenContent x ) $  -- remove \n
                    removeEmpty $ -- remove everything (space, tabs) except space
                    tokenize str  -- tokenize


-- Please pass in pure content, no line breaks.
interpreteTopLevel :: [Token] -> [Declaration]
--interpreteTopLevel tokens | trace ("Called interpreteTopLevel with tokens = " ++ show tokens) False = undefined
interpreteTopLevel [] = []
interpreteTopLevel (x:xs) = if getTokenType x == Comment then interpreteTopLevel xs -- ignore comments
                            else 
                                case getTokenContent x of
                                "package" -> let (pkgNameTks, rest) = seekTokenForward xs ";"
                                             in Package ( concat $ map getTokenContent pkgNameTks)  -- use init to get rid of ;
                                                        : interpreteTopLevel rest
                                "import" -> let isStatic = (getTokenContent $ head xs ) == "static"  
                                                remaining = if isStatic -- skip static
                                                           then tail xs 
                                                           else xs
                                                (importName, rest) = seekTokenForward remaining ";"
                                             in Import  ( concat $ map getTokenContent importName) isStatic -- use init to get rid of ;
                                                        : interpreteTopLevel rest
                                -- annotations
                                "@" -> interpreteAnnotation xs
                                --_ -> if (isModifier $ getTokenContent x) 
                                --    then interpreteModifier (x:xs) interpreteTopLevel
                                --    else  error $ "Unrecognized token starting" ++ show (x:xs)
                                --Assume interprete modifier method to do the rest
                                _ -> interpreteModifier (x:xs) interpreteTopLevel
                            

isModifier :: String -> Bool
isModifier str = str `elem` ["public", "static", "private", "protected", "final", "abstract", "synchronized"] -- volatile not added

-- this method is responsible for parsing class, field, and method declarations
interpreteModifier :: [Token] -- all remaining tokens, including modifiers
                      -> ([Token] -> [Declaration]) -- the function to delegates back when class field or instance has done interpretation
                      -> [Declaration] -- all remaining declarations
--interpreteModifier tks func | trace ("interpreting modifier" ++ show ( head tks ) ) False = undefined
interpreteModifier tks func = interpreteModifierRec [] tks
    where interpreteModifierRec :: [Token] -- modifiers so far
                                   -> [Token] -- remaining tokens
                                   -> [Declaration] -- result of Rec interpretation
          interpreteModifierRec tks [] = error $ "unexpected ending following modifier sequence " ++ show tks
          interpreteModifierRec tks (x:xs)
                -- ignore comments
                | getTokenType x == Comment = interpreteModifierRec tks xs
                | isModifier (getTokenContent x) = interpreteModifierRec (tks ++ [x]) xs
                | getTokenContent x == "class" = let (classDefTks, remaining) = seekTokenForward xs "{" -- extract difinition
                                                     (classBodyTks, rest) = matchParenthesis remaining Brace -- extract body 
                                                 in interpreteClassDeclaration classDefTks classBodyTks tks
                                                    : trace ("Created class " ++ (show classDefTks) ++ " Continuing at " ++ (show $ take 5 rest)) 
                                                        (func rest)
                 -- assume identifier, see if it is a variable or a function
                 -- constructor
                | getTokenContent (head xs) == "(" = let funcName = x
                                                         (params, remaining) = matchParenthesis (tail xs) Paren-- one drop (0) -> left paren
                                                         (excepTks, remaining2) = seekTokenForward remaining "{" -- exceptions
                                                         (functionBody, rest) = matchParenthesis remaining2 Brace -- tail to drop "{"
                                                  in interpreteConstructorDeclaration tks funcName params excepTks functionBody
                                                    : func rest
                -- protection
                | length xs < 2 = error $ "Unrecognized token " ++ show (x:xs)
                 -- function 
                 -- !!1 means (head . tail)
                | getTokenContent (xs !! 1) == "(" = let returnVal = x 
                                                         funcName = head xs 
                                                                                 --TWO DROPS (0) -> funcName (1) -> left paren
                                                         (params, remaining)  = matchParenthesis (drop 2 xs) Paren -- extract parameters 
                                                         (excepTks, remaining2) = seekTokenForward remaining "{"
                                                         (functionBody, rest) = matchParenthesis remaining2 Brace -- extract function body, tail to drop "{"
                                                     in interpreteFunctionDeclaration tks returnVal funcName params excepTks functionBody
                                                        : func rest
                -- local variable
                | getTokenContent (xs !! 1) == ";" 
                   || getTokenContent (xs !! 1) == ","
                   || getTokenContent (xs !! 1) == "="  =  let typeTk = x
                                                           in case getTokenContent (xs !! 1) of
                                                                ";" -> let varName = head xs 
                                                                        in interpreteFieldDeclaration tks typeTk varName Nothing 
                                                                            : func (drop 2 xs) -- TWO DROPS (0) -> var name, (1) -> ;
                                                                -- seperate by comma, and initialize
                                                                _ -> let (allFieldTokens, rest) = seekUntilNextUnparenthesizedCharacter xs ";" 
                                                                            -- seperate tokens by space
                                                                         fieldDeclarations = seperateByNoneParenthesizedCharacter allFieldTokens ","
                                                                            


                                                                       in trace ( "Got Field Declarations: " ++ show fieldDeclarations) $
                                                                         (flip map) fieldDeclarations (\fieldToken -> 
                                                                            trace ("Mapping filedToken " ++ show fieldToken) $
                                                                            let varName = head fieldToken
                                                                            in 
                                                                            -- remove DEBUG LEGACY --
                                                                            --if length fieldToken ==0 
                                                                            --then interpreteFieldDeclaration tks typeTk varName Nothing
                                                                            --else
                                                                            -- remove ABOVE

                                                                               if length fieldToken == 1 
                                                                               then interpreteFieldDeclaration tks typeTk varName Nothing 
                                                                               else if getTokenContent(fieldToken !! 1) == "="
                                                                                    then let initTks = drop 2 fieldToken
                                                                                         in interpreteFieldDeclaration tks typeTk varName (Just initTks)
                                                                                    else error $ "Expected token string " ++ show fieldToken
                                                                       
                                                                       
                                                                       
                                                                       ) ++ func rest

                | otherwise = error $ "Unrecognized Token" ++ show x ++ "   Rest   " ++ show xs

-- seperate a token by non parenthesized characer, 
seperateByNoneParenthesizedCharacter :: [Token] -> String -> [[Token]]
seperateByNoneParenthesizedCharacter [] _ = []
seperateByNoneParenthesizedCharacter tks str = let (cur, rest) = seekUntilNextUnparenthesizedCharacter  tks str
                                           in cur : seperateByNoneParenthesizedCharacter rest str
seekUntilNextUnparenthesizedCharacter :: [Token] -- input token
                                        -> String -- the token to see, 
                                        -> ([Token], [Token]) -- the first contains all tokens EXCLUDING until the 
                                                              -- the second contains all tokens left, EXCLUDING the desired token
seekUntilNextUnparenthesizedCharacter tks str = seekUntilNextUnparenthesizedCharacterRec ([], tks) 
     where  seekUntilNextUnparenthesizedCharacterRec :: ([Token], [Token]) -> ([Token], [Token])
            seekUntilNextUnparenthesizedCharacterRec (a, [] )  = (a, [])
            -- TODO IMPORTANT RECONSIDER DESIGN --
            seekUntilNextUnparenthesizedCharacterRec (a,(x:xs)) = case getTokenContent x of
                            "(" -> let (internal, remaining)  = matchParenthesis xs Paren
                                   in seekUntilNextUnparenthesizedCharacterRec (a ++ (x:internal) ++ [(Token ")" SpecialCharacter)], remaining)
                            "[" -> let (internal, remaining)  = matchParenthesis xs Bracket
                                   in seekUntilNextUnparenthesizedCharacterRec (a ++ (x:internal) ++ [(Token "]" SpecialCharacter)], remaining)
                            "{" -> let (internal, remaining)  = matchParenthesis xs Brace
                                   in seekUntilNextUnparenthesizedCharacterRec (a ++ (x:internal) ++ [(Token "}" SpecialCharacter)], remaining)
                            c -> if c == str then (a, xs) else (a ++ [x] , xs)

interpreteFieldDeclaration :: [Token] -- Annotation
                              -> Token -- Type
                              -> Token -- Name
                              -> Maybe [Token] -- initialization Statement
                              -> Declaration
interpreteFieldDeclaration annotationTks typeTk nameTk initStmtMaybe = Field "NOT IMPLEMENTED" "NOT IMPLEMENTED" [] Nothing

-- helper method to Interpret function
interpreteFunctionDeclaration :: [Token] -- Annotation
                              -> Token -- Return Type
                              -> Token -- Name
                              -> [Token] -- Parameter Tokens
                              -> [Token] -- Exceptions
                              -> [Token] -- Function Body
                              -> Declaration -- the method declaration
interpreteFunctionDeclaration  annotationTks typeTk nameTk paramTks excepTks funcBodyTks= Method "NOT IMPLEMENTED" "NOT IMPLEMENTED" [] [] [] []

-- helper method to Interpret function
interpreteConstructorDeclaration :: [Token] -- Annotation
                              -> Token -- Name
                              -> [Token] -- Parameter Tokens
                              -> [Token] -- Function Body
                              -> [Token] -- Exceptions
                              -> Declaration -- the constructor declaration
interpreteConstructorDeclaration  annotationTks nameTk paramTks excepTks funcBodyTks= Constructor  "NOT IMPLEMENTED" [] [] [] []


interpreteClassBody :: [Token] -> [Declaration]
interpreteClassBody (x:xs) = interpreteTopLevel (x:xs)
                       -- case getTokenContent x of 
                       --         "@" -> interpreteAnnotation xs
                       --         --Assume interprete modifier method to do the rest
                       --         _ -> interpreteModifier (x:xs) interpreteClassBody

-- help function to interpret (arg2) class (arg0) { (arg1) }
interpreteClassDeclaration :: [Token] -- class declaration line excluding class and opening curly braces
                              -> [Token] -- class body definition, excluding { and }
                              -> [Token] -- modifiers
                              -> Declaration

interpreteClassDeclaration classDefTks classBodyTks modifierTks
            =  let className = getTokenContent $ head classDefTks
                   superclassName = case seekTokenForwardMaybe classDefTks "extends"
                                        of Just (_, rest) -> getTokenContent $ head rest -- the superclass is the first token in rest
                                           Nothing -> "Object"
                   interfaces = case seekTokenForwardMaybe classDefTks "implements"
                                        of Just (_, infTks) -> LSplit.splitOn "," $ concat $ map getTokenContent infTks -- get the tokens after implements keyword, split by ,
                                           Nothing -> []
                   bodies = interpreteClassBody classBodyTks
               in ClassDefinition className superclassName (map getTokenContent modifierTks) interfaces bodies

-- this method interpretes a token. It delegates back to interprete top level after it finishes
interpreteAnnotation  :: [Token] -- remaining tokens excluding Token "@"
                         -> [Declaration] -- the interpreted declarations after delegating back to interpret top level
interpreteAnnotation [] = error "expected identifier after annotation symbol @"
interpreteAnnotation [a] = [Annotation (getTokenContent a) ""] -- this is an ERROR, Annotations not allowd at file end. 
                                                               -- This implementation ignore this error.
interpreteAnnotation (x:y:xs)
                       | getTokenContent y ==  "(" = let (annotationContent, rest) = matchParenthesis xs Paren
                                                     in interpreteAnnotationContent [x] annotationContent
                                                           : interpreteTopLevel rest
                       | otherwise      = let annotationName = getTokenContent x
                                          in Annotation annotationName "" : interpreteTopLevel (y:xs) -- y is not paren, preserve

-- helper function to interpret @Annotation(Content)
interpreteAnnotationContent :: [Token] -- name 
                               -> [Token] -- content tokens
                               -> Declaration -- annotation declaration
interpreteAnnotationContent nameTks contentTks = Annotation (concat $ map getTokenContent nameTks)
                                                            (concat $ map getTokenContent contentTks)





data Declaration = Import { getImportPath ::ImportPath -- the path to import
                          , isImportStatic :: Bool -- whether the import is static
                          } 
                 | Package { getPackageName :: PackageName}
                 | ClassDefinition { getClassName :: ClassName -- the class name, Use className$1, className$2 if Anonymous
                                   , getSuperClassname :: SuperclassName  -- the super class name, Object if not supplied
                                   , getModifiers :: [Modifier] -- the modifiers
                                   , getInterfaceNames :: [InterfaceName]  -- the implementing interfaces, empty if not supplied
                                   , getDeclarations:: [Declaration] -- the declaration inside a class
                                   }
                 | Field Type  -- the type of the field
                         Name  -- the name of the field
                         [Modifier] -- Method Modifiers
                         (Maybe Statement) -- Initialization statement
                 | Constructor  -- java contructor
                          Name  -- name of constructor
                          [Modifier] -- modifier of constructor
                          [Declaration]  -- parameter list
                          [Exception] -- the exception this function throws
                          [Statement] -- statements
                 | Parameter Type Name
                 | Method Type -- the type of the field
                          Name -- the name of the field
                          [Modifier] -- the modifiers
                          [Declaration] -- the parameter declaration
                          [Exception] -- the exception this function throws
                          [Statement] -- method body
                 | Annotation { getAnnotationName :: String  -- name
                              , getAnnotationContent :: AnnotationContent -- empty if no parens
                              }
                          deriving (Show)


type ImportPath = String
type PackageName = String
type ClassName = String
type SuperclassName = String
type InterfaceName = String
type Type = String
type Name = String
type Modifier = String
type Annotation = String
type AnnotationContent = String
type Exception = String


data Statement = EmptyStatement 
                          deriving (Show)



data ParenthesisType = Paren -- left : '(', right : ')'
                     | Bracket -- left :: '[', right : ']'
                     | Brace -- left :: '{', right: '}'

left :: ParenthesisType -> Token
left Paren = Token "(" SpecialCharacter
left Bracket = Token "[" SpecialCharacter
left Brace = Token "{" SpecialCharacter

right :: ParenthesisType -> Token
right Paren = Token ")" SpecialCharacter
right Bracket = Token "]" SpecialCharacter
right Brace = Token "}" SpecialCharacter


matchParenthesis :: [Token] -- the tokens NOT including the starting parenthesis
                    -> ParenthesisType -- the parenthesis type
                    -> ([Token], [Token]) -- left contains all tokens from start until (NOT INCLUDING) the terminating parenthesis
                                          -- Remaining tokens (NOT INCLUDING) the terminating parenthesis
matchParenthesis tks tp = matchParenthesisRec 1 ([],  tks)
     where matchParenthesisRec :: Int  -- number of left parenthesis not matched so far
                                  -> ([Token], [Token]) -- move one byte from snd to fst 
                                  -> ([Token], [Token]) -- result after moving
           matchParenthesisRec count (a, b) | trace ("Called matchParenthesisRec on count = " ++ show count ++ " a = " ++ show a ++ " b = " ++ show b) False = undefined
           matchParenthesisRec 0 (a,b) = (init a,b) -- use init to drop trailing parenthesis
           matchParenthesisRec count (a,[]) = error $ "cannot match parenthesis " ++ show (right tp) ++ " expected "
           matchParenthesisRec count (a,(x:xs))
                                    | x == left tp = matchParenthesisRec (count+1) (a ++ [x], xs)
                                    | x == right tp = matchParenthesisRec (count-1) (a ++ [x], xs)
                                    | otherwise = matchParenthesisRec count (a++[x], xs)


seekTokenForwardMaybe :: [Token] -> String -> Maybe ([Token], [Token])
seekTokenForwardMaybe tks until = seekTokenForwardRec ([], tks)
        where seekTokenForwardRec (a, []) = Nothing
              seekTokenForwardRec (a, (x:xs)) = if getTokenContent x  == until
                                                then Just (a, xs) 
                                                else seekTokenForwardRec (a ++ [x], xs)

seekTokenForward :: [Token] -> String -> ([Token], [Token]) -- split token at given string, fst contains everything until given string token, (EXCLUDING), 
                                                            -- snd contains the remaining (EXCLUDING given string token)
                                                          
seekTokenForward tks until = case seekTokenForwardMaybe tks until of 
                                        Just a -> a
                                        Nothing -> error $ "expected " ++ show until


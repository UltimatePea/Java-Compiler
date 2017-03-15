import Data.Char
import System.IO;
import Tokenizer;
import Debug.Trace;



main = do  inputStr <- hGetContents stdin
           putStrLn $ process inputStr



process :: String -> String
process str = show $ interpreteTopLevel $ 
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
                                _ -> if (isModifier $ getTokenContent x) 
                                    then interpreteModifier (x:xs) 
                                    else  error $ "Unrecognized token starting" ++ show (x:xs)
                            

isModifier :: String -> Bool
isModifier str = str `elem` ["public", "static", "private", "protected", "final", "abstract", "synchronized"] -- volatile not added

-- this method is responsible for parsing class, field, and method declarations
interpreteModifier :: [Token] -- all remaining tokens, including modifiers
                      -> [Declaration] -- all remaining declarations

interpreteModifier tks = interpreteModifierRec [] tks
    where interpreteModifierRec :: [Token] -- modifiers so far
                                   -> [Token] -- remaining tokens
                                   -> [Declaration] -- result of Rec interpretation
          interpreteModifierRec tks [] = error $ "unexpected ending following modifier sequence " ++ show tks
          interpreteModifierRec tks (x:xs)
                | isModifier (getTokenContent x) = interpreteModifierRec (tks ++ [x]) xs
                | getTokenContent x == "class" = let (classDefTks, remaining) = seekTokenForward xs "{" -- extract difinition
                                                     (classBodyTks, rest) = matchParenthesis remaining Brace -- extract body 
                                                 in interpreteClassDeclaration classDefTks classBodyTks tks
                                                    : interpreteTopLevel rest


interpreteClassDeclaration :: [Token] -- class declaration lien excluding class and opening curly braces
                              -> [Token] -- class body definition, excluding { and }
                              -> [Token] -- modifiers
                              -> Declaration

interpreteClassDeclaration classDefTks classBodyTks modifierTks= ClassDefinition "Pending" "Pending" (map getTokenContent modifierTks) [] []

-- this method interpretes a token. It delegates back to interprete top level after it finishes
interpreteAnnotation  :: [Token] -- remaning tokens excluding Token "@"
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

interpreteAnnotationContent :: [Token] -- name 
                               -> [Token] -- content tokens
                               -> Declaration -- annotation declaration
interpreteAnnotationContent nameTks contentTks = Annotation (concat $ map getTokenContent nameTks)
                                                            (concat $ map getTokenContent contentTks)





data Declaration = Import ImportPath -- the path to import
                          Bool -- whether the import is static
                 | Package PackageName
                 | ClassDefinition ClassName -- the class name, Use className$1, className$2 if Anonymous
                                   SuperclassName  -- the super class name, Object if not supplied
                                   [Modifier] -- the modifiers
                                   [InterfaceName]  -- the implementing interfaces, empty if not supplied
                                   [Declaration] -- the declaration inside a class
                 | Field Type  -- the type of the field
                         Name  -- the name of the field
                        [Modifier] -- Method Modifiers
                        Statement -- Initialization statement
                 | Parameter Type Name
                 | Method Type -- the type of the field
                          Name -- the name of the field
                          [Modifier] -- the modifiers
                          [Declaration] -- the parameter declaration
                          [Statement] -- method body
                 | Annotation String  -- name
                              AnnotationContent -- empty if no parens
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
           --matchParenthesisRec count (a, b) | trace ("Called matchParenthesisRec on count = " ++ show count ++ " a = " ++ show a ++ " b = " ++ show b) False = undefined
           matchParenthesisRec 0 (a,b) = (init a,b) -- use init to drop trailing parenthesis
           matchParenthesisRec count (a,[]) = error $ "cannot match parenthesis " ++ show (right tp) ++ " expected"
           matchParenthesisRec count (a,(x:xs))
                                    | x == left tp = matchParenthesisRec (count+1) (a ++ [x], xs)
                                    | x == right tp = matchParenthesisRec (count-1) (a ++ [x], xs)
                                    | otherwise = matchParenthesisRec count (a++[x], xs)

seekTokenForward :: [Token] -> String -> ([Token], [Token]) -- split token at given string, fst contains everything until given string token, (EXCLUDING), 
                                                            -- snd contains the remaining (EXCLUDING given string token)
seekTokenForward tks until = seekTokenForwardRec ([], tks)
        where seekTokenForwardRec (a, []) = error $ "expected " ++ show until
              seekTokenForwardRec (a, (x:xs)) = if getTokenContent x  == until
                                                then (a, xs) 
                                                else seekTokenForwardRec (a ++ [x], xs)

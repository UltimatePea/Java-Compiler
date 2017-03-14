
import System.IO;
import Tokenizer;


main = do  inputStr <- hGetContents stdin
           putStrLn $ process inputStr



process :: String -> String
process str = concat $ map getTokenContent $ tokenize str

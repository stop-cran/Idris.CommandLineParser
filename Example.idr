module Main

import CommandLineParser
import Lightyear
import Lightyear.Combinators
import Lightyear.Core
import Lightyear.Char
import Lightyear.Strings
import System

%access public export


record Arguments where
    constructor MkArguments
    input: List String
    output: List String
    num: Nat

fromDigits : List (Fin 10) -> Nat
fromDigits = foldl (\a, b => 10 * a + cast b) 0

parseNat : Parser Nat
parseNat = do digits <- some digit
              eof
              pure $ fromDigits digits

argumentsParser : Monad m => ParserT (List String) m Arguments
argumentsParser = do
    -- Parse a list of string parameters
    i <- (key "-i" *> (manyTill anyToken (key "-o"))) <?> "input"
    -- Parse another list of string parameters
    o <- manyTill anyToken (key "-n") <?> "output"
    -- Parse a single typed value
    n <- (value parseNat) <* eof <?> "num"
    pure $ MkArguments i o n

main : IO ()
main = do
    (path :: args) <- getArgs
    case (parseList argumentsParser args) of
        Left error => do
            putStrLn $ "Error: " ++ error
            exit 1
        Right (MkArguments i o n) => do
            putStrLn "Success:"
            putStrLn $ "input: " ++ (show i)
            putStrLn $ "output: " ++ (show o)
            putStrLn $ "num: " ++ (show n)

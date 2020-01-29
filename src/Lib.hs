module Lib
    ( readExpr
    )
where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment
import           Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> digit)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = do
    num <- fmap read $ many1 digit
    return $ Number num



parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- | Strip whitepace, throws out one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

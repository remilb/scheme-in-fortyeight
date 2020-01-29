module Scheme
    ( LispVal(..)
    , LispError(..)
    , ThrowsError
    )
where

import           Text.ParserCombinators.Parsec  ( ParseError )

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool

data LispError = NumArgs Integer [LispVal]
            | TypeMismatch String LispVal
            | ParseError ParseError
            | BadSpecialForm  String LispVal
            | NotFunction String String
            | UnboundVar String String
            | Default String

type ThrowsError = Either LispError

-- I belive this is technically an abuse of Show
instance Show LispVal where
    show = showVal

instance Show LispError where
    show = showError


showVal :: LispVal -> String
showVal (Atom   name) = name
showVal (String str ) = "\"" ++ str ++ "\""
showVal (Number n   ) = show n
showVal (Bool   b   ) = if b then "#t" else "#f"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (List vals) = "(" ++ unwordsList vals ++ ")"


showError :: LispError -> String
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (ParseError parseErr           ) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction    message func   ) = message ++ ": " ++ show func
showError (UnboundVar     message varname) = message ++ ": " ++ varname



unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



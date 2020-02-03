module Scheme where

import           Text.ParserCombinators.Parsec  ( ParseError )
import           Control.Monad.Except
import           Data.IORef
import           System.IO                      ( Handle )

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func {params :: [String], vararg :: (Maybe String),
                    body :: [LispVal], closure :: Env}
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Port Handle

data LispError = NumArgs Integer [LispVal]
            | TypeMismatch String LispVal
            | ParseError ParseError
            | BadSpecialForm  String LispVal
            | NotFunction String String
            | UnboundVar String String
            | Default String

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val


trapError err = catchError err (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


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
showVal (List          vals) = "(" ++ unwordsList vals ++ ")"
showVal (PrimitiveFunc prim) = "<primitive>"
showVal (Func params vararg body closure) =
    "(lambda ("
        ++ unwords (map show params)
        ++ (case vararg of
               Nothing  -> ""
               Just arg -> " . " ++ arg
           )
        ++ ") ...)"
showVal (Port   _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


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

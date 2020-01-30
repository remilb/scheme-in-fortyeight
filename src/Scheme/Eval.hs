module Scheme.Eval where

import           Scheme
import           Control.Monad.Except

-- trapError :: ThrowsError a -> ThrowsError String
trapError err = catchError err (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                             ) = return val
eval val@(Number _                             ) = return val
eval val@(Bool   _                             ) = return val
eval (    List   [Atom "quote", val]           ) = return val
eval (    List   [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)        ] = return x
car [DottedList (x : xs) _] = return x
car [badArg               ] = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList []       x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x , List []           ] = return $ List [x]
cons [x , List xs           ] = return $ List (x : xs)
cons [x , DottedList xs last] = return $ DottedList (x : xs) last
cons [x1, x2                ] = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool   arg1), (Bool arg2)  ] = return $ Bool (arg1 == arg2)
eqv [(Number arg1), (Number arg2)] = return $ Bool (arg1 == arg2)
eqv [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
eqv [(Atom   arg1), (Atom arg2)  ] = return $ Bool (arg1 == arg2)
eqv [(DottedList xs1 t1), (DottedList xs2 t2)] =
    eqv [List $ xs1 ++ [t1], List $ xs2 ++ [t2]]
eqv [(List arg1), (List arg2)] =
    return
        $  Bool
        $  (length arg1 == length arg2)
        && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
        Left  err        -> False
        Right (Bool val) -> val
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe
            ( throwError
            $ NotFunction "Unrecognized primitive function args" func
            )
            ($ args)
        $ lookup func primitives


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"        , numericBinop (+))
    , ("-"        , numericBinop (-))
    , ("*"        , numericBinop (*))
    , ("/"        , numericBinop div)
    , ("mod"      , numericBinop mod)
    , ("quotient" , numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("="        , numBoolBinop (==))
    , ("<"        , numBoolBinop (<))
    , (">"        , numBoolBinop (>))
    , ("/="       , numBoolBinop (/=))
    , (">="       , numBoolBinop (>=))
    , ("<="       , numBoolBinop (<=))
    , ("&&"       , boolBoolBinop (&&))
    , ("||"       , boolBoolBinop (||))
    , ("string=?" , strBoolBinop (==))
    , ("string<?" , strBoolBinop (<))
    , ("string>?" , strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    ]


numericBinop
    :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op


boolBinop
    :: (LispVal -> ThrowsError a)
    -> (a -> a -> Bool)
    -> [LispVal]
    -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        arg1 <- unpacker $ args !! 0
        arg2 <- unpacker $ args !! 1
        return $ Bool (arg1 `op` arg2)


numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n :: [(Integer, String)]
    in  if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return (show n)
unpackStr (Bool   b) = return (show b)
unpackStr notStr     = throwError $ TypeMismatch "string" notStr


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Eval where

import           Scheme
import           Control.Monad.Except
import           Data.IORef

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- trapError :: ThrowsError a -> ThrowsError String
trapError err = catchError err (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                             ) = return val
eval env val@(Number _                             ) = return val
eval env val@(Bool   _                             ) = return val
eval env (Atom id) = getVar env id
eval env (    List   [Atom "quote", val]           ) = return val
eval env (    List   [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

-- Function definition
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
-- Function application
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varArgs env params body = return $ Func (map show params) varArgs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

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


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        num = toInteger. length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of 
                                    Just argName -> let vargs = drop (length params) args 
                                                in liftIO $ bindVars env [(argName, List vargs)]
                                    Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (name, func) = (name, PrimitiveFunc func)

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
    , ("car"      , car)
    , ("cdr"      , cdr)
    , ("cons"     , cons)
    , ("eq?"      , eqv)
    , ("eqv?"     , eqv)
    , ("equal?"   , equal)
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


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do
            res1 <- unpacker arg1
            res2 <- unpacker arg2
            return $ res1 == res2
        `catchError` (const $ return False)


{- ### Mutable Variables Stuff  ### -}
nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound env name = do
    bindings <- readIORef env
    return $ any ((== name) . fst) bindings

getVar :: Env -> String -> IOThrowsError LispVal
getVar env name = do
    bindings <- liftIO $ readIORef env
    maybe
        (throwError $ UnboundVar "Getting an unbound variable with name: " name)
        (liftIO . readIORef)
        (lookup name bindings)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar env name value = do
    bindings <- liftIO $ readIORef env
    maybe (throwError $ UnboundVar "Setting an unbound variable: " name)
          (liftIO . (flip writeIORef value))
          (lookup name bindings)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar env name value = do
    alreadyDefined <- liftIO $ isBound env name
    if alreadyDefined
        then setVar env name value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            bindings <- readIORef env
            writeIORef env ((name, valueRef) : bindings)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (name, value) = do
        ref <- newIORef value
        return (name, ref)



type IOThrowsError = ExceptT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

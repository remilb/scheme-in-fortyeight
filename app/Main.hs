module Main where

import           Scheme
import           Scheme.Parse
import           Scheme.Eval
import           System.Environment
import           Control.Monad


main :: IO ()
main = do
    args   <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

module Main where


import           Repl
import           System.Environment
import           Control.Monad


main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args


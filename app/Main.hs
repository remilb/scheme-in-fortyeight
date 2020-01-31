module Main where


import           Repl
import           System.Environment
import           Control.Monad


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne (head args)
        _ -> putStrLn "Program takes 0 or 1 argument"


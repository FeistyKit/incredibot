
module Main where

import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

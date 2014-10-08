
module Main where

import Data.Stdf
import Data.Stdf.WaferMap
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)

printUsage = putStrLn "usage: WaferMap file.stdf"

main = do
    args <- getArgs
    when (length args /= 1) $ do
        printUsage
        exitFailure
    let file = head args
    putStrLn $ "File: " ++ file
    raw <- BL.readFile file
    putStrLn $ stdfToWaferMapString (parse raw)

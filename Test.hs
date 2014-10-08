

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
    print args
    raw <- BL.readFile $ head args
    putStrLn $ stdfToWaferMapString (parse raw)


module Main where

import Data.Stdf
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Aeson

printUsage = putStrLn "usage: StdfToJson file.stdf"

-- unparsed records have type Raw for the time being
notRaw :: Rec -> Bool
notRaw (Raw _) = False
notRaw _       = True

main = do
    args <- getArgs
    when (length args /= 1) $ do
        printUsage
        exitFailure
    let file = head args
    raw <- BL.readFile file
    let recs = parse raw
    let goodRecs = filter notRaw recs
    mapM_ (BL.putStrLn . encode) goodRecs

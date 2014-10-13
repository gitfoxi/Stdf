
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

isFtr :: Rec -> Bool
isFtr (Ftr{}) = True
isFtr _ = False

main = do
    args <- getArgs
    when (length args /= 1) $ do
        printUsage
        exitFailure
    let file = head args
    recs <- parseFile file
    let goodRecs = filter notRaw recs
    mapM_ (BL.putStrLn . encode) goodRecs

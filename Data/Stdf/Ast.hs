
{-# LANGUAGE DeriveGeneric #-}
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Stdf.Parser as SP
import           Data.Stdf.Parser (u4, u1, i1, c1)
import qualified Data.Stdf.Types as ST
import           Data.Stdf.Types (Header(..), Rec(..), BinRec(..), rec, U1, C1, U2)
import Data.Binary.Get hiding (Fail)
import qualified Data.ByteString.Lazy.Char8 as BL
import Codec.Compression.GZip
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Applicative
import Data.Bits (testBit)
import GHC.Generics hiding (U1, C1)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64.Lazy as Base64

-- should I have expressions and what they evaluate to?
-- For example: Vdd = VddNom + 0.006 -> 0.9006

type Facts = HashMap Name Value

data Ast = Let [Binding] Ast -- Open a context
         | In  [Ast]        -- Logged within context
          -- Unspecial facts
         | Facts Facts
          -- Test could be a contentious name
         | FunctionalTest Id PassFail Facts
         | ParametricTest Id PassFail Result Facts
         | MultiParametricTest Id PassFail [Result] Facts
         | Location (Head, Site) [Ast] -- should Location just be a Fact?
        deriving Show

data Binding = Bind Name Value
             | Eval Name Expr
             deriving Show


data Expr = EFloat Float
          | BinOp Expr Expr
             deriving Show

data BinOp = Add
           | Sub
           | Mul
           | Div
             deriving Show

data Value = VInt Int
           | VTime UTCTime
           | VStr Text
           | VFloat Float
           | VList [Value]
             deriving Show

type Name = Text

-- One Id type or many?
type Id = Int

data PassFail = Alarm       -- bit 0
              | Reserved    -- bit 1
              | Unreliable  -- bit 2
              | Timeout     -- bit 3
              | NotExecuted -- bit 4
              | Aborted     -- bit 5
              | InvalidPassFail     -- bit 6
              | Pass        -- bit 7 == 0
              | Fail        -- bit 7
              deriving (Generic, Show, Eq, Enum)

-- bit 1 of TEST_FLG only applies to MultiParametricTest 
-- tests may Pass or Fail independantly of whether the result is valid
data Result = InvalidResult
            | Result PinId Float Units-- Maybe reference an actual pin later
             deriving Show

type Units = Text

type PinId = Int
data Pin = Pin PinId Ast
         | PinGroup PinId Name [PinId]
--                  ^ Pin and PinGroup share numbering so you can nest groups
             deriving Show


type Head = Int
type Site = Int

getLocation :: Get (Head, Site)
getLocation = do
    headId <- fromIntegral <$> u4
    siteId <- fromIntegral <$> u4
    return (headId, siteId)

-- TODO: Try Get Monad
getFtr :: Get Ast
getFtr = do
    testId <- u4
    loc <- getLocation
    res <- getPassFail

    let facts = Facts H.empty -- TODO: unspecial facts
    let ftr = FunctionalTest testId res facts
    return $ Location loc ftr
-- --------------------------------------------------------------
-- Copy pasta
--

-- Index of the first high bit 0-7. If somehow they are all low,
-- return 8
firstHighBit :: U1 -> Int
firstHighBit byte = fh byte 0
        where fh 8 = 8
              fh bit = if testBit byte bit then bit else fh (bit + 1)

-- I don't really want a list of flags. The first high one
-- will tell me what went wrong and the rest seem irrelevant
decodeTestFlags :: U1 -> PassFail
decodeTestFlags = mapTf firstHighBit
                     where
                         mapTf 0 = Alarm
                         mapTf 1 = Reserved    -- bit 1
                         mapTf 2 = Unreliable  -- bit 2
                         mapTf 3 = Timeout     -- bit 3
                         mapTf 4 = NotExecuted -- bit 4
                         mapTf 5 = Aborted     -- bit 5
                         mapTf 6 = InvalidPassFail     -- bit 6
                         mapTf 7 = Fail        -- bit 7
                         mapTf 8 = Pass        -- bit 7

getPassFail :: Get PassFail
getPassFail = decodeTestFlags <$> u1

getRawRec :: Integral a => a -> Get Rec
getRawRec len = do
    bytes <- getLazyByteString (fromIntegral len)
    return Raw { raw = (decodeUtf8 . Base64.encode) bytes }

-- First get the 'len' number of bytes
-- Although you could just parse the stream directly you would be
-- disapointed when a buggy STDF had some fields of the wrong width
-- in records you didn't even care about and you lose the ability
-- to read the rest of the stream forever
getRec :: ST.Header -> Get Ast
getRec hdr = do
    record <- getLazyByteString (fromIntegral $ len hdr)
    return $ processRec hdr record

processRec :: ST.Header -> BL.ByteString -> Ast
processRec hdr = runGet (specificGet hdr)

-- Attach handlers for specific headers here
specificGet :: ST.Header -> Get Ast
specificGet (ST.Header _ 15 20) = getFtr
specificGet (ST.Header len _ _) = getRawRec len

getBinRec :: Get BinRec
getBinRec = do
    hdr <- getHeader
    record <- getRec hdr
    return $! BinRec hdr record

getHeader :: Get Header
getHeader = ST.Header <$> getWord16le <*> getWord8 <*> getWord8

nextRec :: Get Ast
nextRec = do
    record <- getBinRec
    return $ rec record

parseStdfAst :: Get Ast
parseStdfAst = In <$> parseIn

parseIn :: Get [Ast]
parseIn = do
    empty <- isEmpty
    if empty
        then return []
        else do node      <- nextRec
                nodes     <- parseIn
                return (node:nodes)

parseFile :: String -> IO Ast
parseFile fn = do
    bs <- BL.readFile fn
    isgz <- SP.isGzipped bs -- -> Util
    let decompressed = decompress bs
    return $ parse $ if isgz then decompressed else bs

parse :: BL.ByteString -> Ast
parse = runGet parseStdfAst

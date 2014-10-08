
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Stdf ( parse
                 , Stdf(..)
                 , Rec(..)
                 ) where

import Data.Binary.Get
import Data.ByteString.Lazy.Char8 as BL hiding (show)
import Data.Word
import Data.Int
import Data.Bits (testBit)
import Foreign.C.Types
import Control.Applicative
import Prelude hiding (show)
import Text.Show
import Control.Monad
import Data.Text.Lazy
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Text.Lazy.Encoding

import GHC.Generics hiding (U1)
import Data.Aeson

-- JSON gotcha: can't encode ByteString
-- Have to convert character strings to Text-latin1
-- Encoding base64 raw binary data

-- I was thinking of supporting the CPU_TYPE for converting numbers
-- with different endianness then I realized I would have to support
-- different floating-point encodings from old Sun and DEC architectures
-- and I was like fuck that. All my testers and all my computers are x86
-- anyway.

type U1 = Word8  -- unsigned 1 byte
type U2 = Word16 -- unsigned 2 bytes
type U4 = Word32 -- unsigned 4 bytes
type I1 = Int8
type I2 = Int16
type I4 = Int32
type R4 = CFloat
type R8 = CDouble

instance ToJSON Rec
instance ToJSON PartFlg

data BinRec = BinRec 
    { header :: Header
    , rec :: Rec } deriving (Generic, Show)

data Header = Header
    { len :: !Word16
    , typ :: !Word8
    , sub :: !Word8
    } deriving (Generic, Show)

type Stdf = [Rec]

data Rec  = Raw { raw :: Text } -- base64 TODO: maybe don't bother. what's this good for?
          | Far { cpuType  :: !U1
               , stdfVer  :: !U1 }
          | Prr { headNum  :: !U1
                , siteNum  :: !U1
                , partFlg  :: !PartFlg
                , numTest  :: !U2
                , hardBin  :: !U2
                , softBin  :: Maybe U2
                , xCoord   :: Maybe I2
                , yCoord   :: Maybe I2
                , testTime :: Maybe U4
                , partID   :: Maybe Text
                , partTxt  :: Maybe Text
                , partFix  :: Maybe Text }
          deriving (Generic, Show)

data PartFlg = PartFlg { supersedesPartId :: Bool -- 1 bit
                       , supersedesXY :: Bool -- 1 bit
                       , abnormalEnd :: Bool
                       , failed :: Bool
                       , noPassFailInfo :: Bool }
                       deriving (Generic, Show)

getFar :: Get Rec
getFar = Far <$> getWord8 <*> getWord8

u1 = getWord8
u2 = getWord16le
u4 = getWord32le
cn = getVarLen

i1 :: Get I1
i1 = fromIntegral <$> u1

i2 :: Get I2
i2 = fromIntegral <$> u2

i4 :: Get I1
i4 = fromIntegral <$> u1

mu2 :: Get (Maybe U2)
mu2 = missing (65535 :: U2) <$> u2

missing :: Eq a => a -> a -> Maybe a
missing n m =
    if m == n
        then Nothing
        else Just m


mi2 :: Get (Maybe I2)
mi2 = do
    x <- i2
    return $ case x of
        (-32768) -> Nothing
        otherwise -> Just x

mu4 :: Get (Maybe U4)
mu4 = do
    x <- u4
    return $ case x of
        0 -> Nothing
        otherwise -> Just x

getVarLen :: Get (Maybe Text)
getVarLen = do
    len <- u1
    case len of
        0         -> return Nothing
        otherwise -> liftM (Just . decodeUtf8) $ getLazyByteString (fromIntegral len)

bit :: U1 -> Int -> Bool
bit = testBit

getPartFlg :: Get PartFlg
getPartFlg = do
    b <- u1
    return PartFlg { supersedesPartId = bit b 0
                   , supersedesXY     = bit b 1
                   , abnormalEnd      = bit b 2
                   , failed           = bit b 3
                   , noPassFailInfo   = bit b 4 }

getPrr :: Get Rec
getPrr = Prr <$> u1 <*> u1 <*> getPartFlg <*> u2 <*> u2 <*> mu2 <*> mi2 <*> mi2 <*> mu4 <*> cn <*> cn <*> cn

getRawRec :: Integral a => a -> Get Rec
getRawRec len = do
    bytes <- getLazyByteString (fromIntegral len)
    return Raw { raw = (decodeUtf8 . Base64.encode) bytes }

-- First get the 'len' number of bytes
-- Although you could just parse the stream directly you would be
-- disapointed when a buggy STDF had some fields of the wrong width
-- in records you didn't even care about and you lose the ability
-- to read the rest of the stream forever
getRec :: Header -> Get Rec
getRec hdr = do
    record <- getLazyByteString (fromIntegral $ len hdr)
    return $ processRec hdr record


-- Attach handlers for specific headers here
processRec :: Header -> ByteString -> Rec
processRec hdr = runGet (specificGet hdr)

specificGet :: Header -> Get Rec
specificGet (Header _ 0 10) = getFar
specificGet (Header _ 5 20) = getPrr
specificGet (Header len _ _) = getRawRec len

getBinRec :: Get BinRec
getBinRec = do
    hdr <- getHeader
    record <- getRec hdr
    return $! BinRec hdr record -- needs to be strict?

getHeader :: Get Header
getHeader = Header <$> getWord16le <*> getWord8 <*> getWord8

nextRec :: Get Rec
nextRec = do
    record <- getBinRec
    return $ rec record


getStdf :: Get Stdf
getStdf = do
    empty <- isEmpty
    if empty
        then return []
        else do record   <- nextRec
                recs <- getStdf
                return (record:recs)

parse :: ByteString -> Stdf
parse = runGet getStdf

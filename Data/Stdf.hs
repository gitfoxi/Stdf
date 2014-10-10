
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Stdf ( parse
                 , Stdf(..)
                 , Rec(..)
                 ) where

import Data.Stdf.Types
import Data.Binary.Get
import Data.ByteString.Lazy.Char8 as BL hiding (show)
import Data.Bits (testBit, (.&.))
import Control.Applicative
import Prelude hiding (show, Left, Right)
import Text.Show
import Control.Monad
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Text.Lazy.Encoding
import Data.Text.Lazy
import GHC.Char
import Data.Sequence (replicateA)
import Data.Ix (range)



-- JSON gotcha: can't encode ByteString
-- Have to convert character strings to Text-latin1
-- Encoding base64 raw binary data

-- I was thinking of supporting the CPU_TYPE for converting numbers
-- with different endianness then I realized I would have to support
-- different floating-point encodings from old Sun and DEC architectures
-- and I was like fuck that. All my testers and all my computers are x86
-- anyway.


getFar :: Get Rec
getFar = Far <$> getWord8 <*> getWord8

getAtr :: Get Rec
getAtr = Atr <$> u4 <*> mcn -- TODO: more parsing for time fields

getMir :: Get Rec
getMir = Mir <$> u4 <*> u4 <*> u1 <*> mc1<*> mc1  <*> mc1 <*> mu2
             <*> mc1 <*> cn <*> cn <*> cn <*> cn <*> cn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn

getMrr :: Get Rec
getMrr = Mrr <$> u4 <*> mc1 <*> mcn <*> mcn

getPcr :: Get Rec
getPcr = Pcr <$> u1 <*> u1 <*> u4 <*> mu4 <*> mu4 <*> mu4 <*> mu4 

getHbr :: Get Rec
getHbr = Hbr <$> u1 <*> u1 <*> u2 <*> u4 <*> mc1 <*> mcn

getSbr :: Get Rec
getSbr = Sbr <$> u1 <*> u1 <*> u2 <*> u4 <*> mc1 <*> mcn

getPmr :: Get Rec
getPmr = Pmr <$> u2 <*> mu2e0 <*> mcn <*> mcn <*> mcn <*> u1 <*> u1

getPgr :: Get Rec
getPgr = Pgr <$> u2 <*> mcn <*> getU2List

-- TODO: 93k stdf have none of this so low-priority
getPlr :: Get Rec
getPlr = return $ Plr [] (Just []) (Just []) (Just []) (Just []) (Just []) (Just [])

getRdr :: Get Rec
getRdr = Rdr <$> getU2List

getSdr :: Get Rec
getSdr = Sdr <$> u1 <*> u1 <*> getU1List <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
-- getSdr = Sdr <$> u1 <*> u1 <*> u1 <*> getU1List <*> replicateA 16 mcn

getWir :: Get Rec
getWir = Wir <$> u1 <*> mu1e255 <*> u4 <*> mcn

getWrr :: Get Rec
getWrr = Wrr <$> u1 <*> mu1e255 <*> u4 <*> u4 <*> mu4 <*> mu4 <*> mu4
             <*> mu4 <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn

getWcr :: Get Rec
getWcr = Wcr <$> mr4 <*> mr4 <*> mr4 <*> getWaferUnits <*> getDirection 
             <*> mi2 <*> mi2 <*> getDirection <*> getDirection

getDirection :: Get (Maybe Direction)
getDirection = charToDirection <$> c1
    where charToDirection 'U' = Just Up
          charToDirection 'D' = Just Down
          charToDirection 'L' = Just Left
          charToDirection 'R' = Just Right
          charToDirection ' ' = Nothing
          charToDirection x = Just $ OtherDirection x

getWaferUnits :: Get (Maybe WaferUnits)
getWaferUnits = numToUnits <$> u1
    where numToUnits 0 = Nothing
          numToUnits 1 = Just Inches
          numToUnits 2 = Just Centimeters
          numToUnits 3 = Just Millimeters
          numToUnits 4 = Just Mils
          numToUnits x = Just $ OtherUnits x

-- TODO: there's a pattern here
getU1List :: Get [U1]
getU1List = do
    cnt <- u1
    let count = fromIntegral cnt
    replicateM count u1

getU2List :: Get [U2]
getU2List = do
    cnt <- u2
    let count = fromIntegral cnt
    replicateM count u2

u1 = getWord8
u2 = getWord16le
u4 = getWord32le

r4 :: Get R4
r4 = fromIntegral <$> u4

i1 :: Get I1
i1 = fromIntegral <$> u1

i2 :: Get I2
i2 = fromIntegral <$> u2

i4 :: Get I1
i4 = fromIntegral <$> u1

mu2, mu2e0 :: Get (Maybe U2)
mu2 = missing (65535 :: U2) <$> u2
mu2e0 = missing (0 :: U2) <$> u2

mu1e0 :: Get (Maybe U1)
mu1e0 = missing (0 :: U1) <$> u1
mu1e255 = missing (255 :: U1) <$> u1

missing :: Eq a => a -> a -> Maybe a
missing n m =
    if m == n
        then Nothing
        else Just m

c1 :: Get Char
c1 = (chr . fromIntegral) <$> u1

mc1 :: Get (Maybe C1)
mc1 = getMaybe c1 ' '

mcn :: Get (Maybe Text)
mcn = getMaybe cn ""

mr4 :: Get (Maybe R4)
mr4 = getMaybe r4 0.0

getMaybe :: Eq a => Get a -> a -> Get (Maybe a)
getMaybe get missingVal = do
    x <- get
    return $ if x == missingVal
             then Nothing
             else Just x

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
        4294967295 -> Nothing
        otherwise -> Just x

cn :: Get Text
cn = do
    len <- u1
    case len of
        0         -> return ""
        otherwise -> liftM decodeUtf8 $ getLazyByteString (fromIntegral len)

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
getPrr = Prr <$> u1 <*> u1 <*> getPartFlg <*> u2 <*> u2 <*> mu2
             <*> mi2 <*> mi2 <*> mu4 <*> mcn <*> mcn <*> mcn

getPir :: Get Rec
getPir = Pir <$> u1 <*> u1

getTsr :: Get Rec
getTsr = Tsr <$> u1 <*> u1 <*> getTestType <*> u4 <*> mu4 <*> mu4 <*> mu4 
             <*> mcn <*> mcn <*> mcn -- TODO: actually get the optional data
             <*> pure Nothing
             <*> pure Nothing
             <*> pure Nothing
             <*> pure Nothing
             <*> pure Nothing
         where 
               getTestType = charToTestType <$> c1
               charToTestType 'P' = Just Parametric
               charToTestType 'F' = Just Functional
               charToTestType 'M' = Just MultiResultParametric
               charToTestType ' ' = Nothing
               charToTestType x = Just $ OtherTestType x

getPtr :: Get Rec
getPtr = do
        ptrTestNum <- u4
        ptrHeadNum <- u1
        ptrSiteNum <- u1
        testFlags <- u1 -- getTestFlags -- u1
        parametricFlags <- u1 -- getParametricFlags -- u1
        result <- r4 -- depends on testFlags:: Maybe R4
        testText <- mcn
        alarmId <- mcn
        optionalInfo <- getOptionalInfo
        let mresult = if validResult testFlags parametricFlags
                        then Just result
                        else Nothing

        return $ Ptr  ptrTestNum
                      ptrHeadNum
                      ptrSiteNum
                      testFlags
                      parametricFlags
                      mresult
                      testText
                      alarmId
                      optionalInfo
        where
            validResult tf pf = (tf .&. 0x3f) .&. (pf .&. 0x3) == 0

getOptionalInfo :: Get (Maybe OptionalInfo)
getOptionalInfo = do
        -- check we're not at the end of the buffer
        noInfo <- isEmpty
        if noInfo 
            then return Nothing 
            else do
                optinfo <- getOptionalInfo'
                return $ Just optinfo

getOptionalInfo' :: Get OptionalInfo
getOptionalInfo' = do
    optFlag <- u1 -- getOptFlag

    let [ invalidResultExp,      -- bit 0
          _,                     -- bit 1
          invalidLowSpecLimit,   -- bit 2
          invalidHighSpecLimit,  -- bit 3
          invalidLowLimit,       -- bit 4
          invalidHighLimit,      -- bit 5
          invalidLowTestLimit,   -- bit 6
          invalidHighTestLimit ] -- bit 7
          = Prelude.map (testBit optFlag) $ range (0,7)

    let invalidLowTest  = invalidLowLimit || invalidLowTestLimit
    let invalidHighTest = invalidHighLimit || invalidHighTestLimit

    OptionalInfo <$> getOnFalse invalidResultExp i1
                 <*> getOnFalse invalidLowTest i1
                 <*> getOnFalse invalidHighTest i1
                 <*> getOnFalse invalidLowTest r4
                 <*> getOnFalse invalidHighTest r4
                 <*> mcn <*> mcn <*> mcn <*> mcn 
                 <*> getOnFalse invalidLowSpecLimit r4
                 <*> getOnFalse invalidHighSpecLimit r4

getOnFalse :: Bool -> Get a -> Get (Maybe a)
getOnFalse cond get =
    if cond
            then return Nothing
            else Just <$> get

getFtr :: Get Rec
getFtr = do
    testNum <- u4
    headNum <- u1
    siteNum <- u1
    testFlag <- u1 -- TODO: decode
    optFlag <- u1

    let [noCycleCnt,
         noRelVadr,
         noReptCnt,
         noNumFail,
         noXYFail,
         noVectOff] = Prelude.map (testBit optFlag) $ range (0, 5)

    cycleCnt <- getOnFalse noCycleCnt u4
    relVadr  <- getOnFalse noRelVadr u4
    reptCnt  <- getOnFalse noReptCnt u4
    numFail  <- getOnFalse noNumFail u4
    xFail    <- getOnFalse noXYFail i4
    yFail    <- getOnFalse noXYFail i4
    vectOff  <- getOnFalse noVectOff i2

    j <- U2
    k <- U2

-- TODO Nothing if j/k == 0
    rtnIndx <- replicateM j u2
    rtnStat <- getNibbles j
    pgmIndx <- replicateM k u2
    pgmStat <- getNibbles k

    failPin <- getBitField
    vecNam  <- mcn
    timeSet <- mcn
    opCode  <- mcn
    testTxt <- mcn
    alarmId <- mcn
    progTxt <- mcn
    rsltTxt <- mcn

    patgNum <- mu1e255
    spinMap <- getBitField

    return $ Ftr testNum headNum siteNum testFlag cycleCnt relVadr reptCnt numFail 
                xFail yFail vectOff rtnIndx rtnStat pgmIndx pgmStat
                failPin vecNam timeSet opCode testTxt alarmId progTxt rsltTxt patgNum spinMap

-- TODO: BitField type like [U1] which toJson prints as hex "FF AF 12 ..."
getBitField :: Get BitField -- [U1]
getBitField = do
    nbits <- u2
    let nbytes = let (mbytes, mbits) = nbits `divMod` 8
                 in mbytes + if mbits == 0 then 0 else 1
    BitField <$> replicateM nbytes u1

getBps :: Get Rec
getBps = Bps <$> mcn

getEps :: Get Rec
getEps = return Eps

getDtr :: Get Rec
getDtr = Dtr <$> cn

----------------------------------------------------------------
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

typStdfInfo    = 0
subFar = 10
subAtr = 20
typPerLot      = 1
subMir = 10
subMrr = 20
subPcr = 30
subHbr = 40
subSbr = 50
subPmr = 60
subPgr = 62
subPlr = 63
subRdr = 70
subSdr = 80
typPerWafer    = 2
subWir = 10
subWrr = 20
subWcr = 30
typPerPart     = 5
subPir = 10
subPrr = 20
typPerTest     = 10
subTsr = 30
typPerTestExec = 15
subPtr = 10
subMpr = 15
subFtr = 20
typPerProgSeg  = 20
subBps = 10
subEps = 20
typGeneric     = 50
subGdr = 10
subDtr = 30

-- Dispatch table for record types
specificGet :: Header -> Get Rec
-- STDF file info
specificGet (Header _ 0 10) = getFar
specificGet (Header _ 0 20) = getAtr
-- Per Lot info
specificGet (Header _ 1 10) = getMir
specificGet (Header _ 1 20) = getMrr
specificGet (Header _ 1 30) = getPcr
specificGet (Header _ 1 40) = getHbr
specificGet (Header _ 1 50) = getSbr
specificGet (Header _ 1 60) = getPmr
specificGet (Header _ 1 62) = getPgr
specificGet (Header _ 1 63) = getPlr
specificGet (Header _ 1 70) = getRdr
specificGet (Header _ 1 80) = getSdr
-- Per Wafer Info
specificGet (Header _ 2 10) = getWir
specificGet (Header _ 2 20) = getWrr
specificGet (Header _ 2 30) = getWcr
-- Per-Part Info
specificGet (Header _ 5 10) = getPir
specificGet (Header _ 5 20) = getPrr
-- Per-Test
specificGet (Header _ 10 30) = getTsr
-- -- Per-Test-Execution
specificGet (Header _ 15 10) = getPtr
-- specificGet (Header _ 15 15) = getMpr
-- specificGet (Header _ 15 20) = getFtr
-- -- Per-Program-Segment
specificGet (Header _ 20 10) = getBps
specificGet (Header _ 20 20) = getEps
-- -- Generic Data
-- specificGet (Header _ 50 10) = getGdr
specificGet (Header _ 50 30) = getDtr

--
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

-- TODO: parseFile which detects and decompresses .gz per the spec

parse :: ByteString -> Stdf
parse = runGet getStdf

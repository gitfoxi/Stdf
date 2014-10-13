
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Refactor this file
-- TODO: Less dependencies. I wonder if there's a tool that can tell me about unused imports
-- TODO: Some tests
-- TODO: Yaml/Xml output
-- TODO: JSON/XML data model for STDF

module Data.Stdf ( parse
                 , parseFile
                 , Stdf(..)
                 , Rec(..)
                 ) where

import Data.Stdf.Types
import Data.Binary.Get hiding (Fail)
import Data.ByteString.Lazy.Char8 as BL hiding (show, elem, notElem, all, concatMap, concat, zipWith, map, head)
import Data.Bits (testBit, (.&.), shiftR)
import Control.Applicative
import Prelude hiding (show, Left, Right)
import qualified Prelude
import Text.Show
import Control.Monad
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Text.Lazy.Encoding
import Data.Text.Lazy hiding (all, concatMap, concat, zipWith, map, head)
import GHC.Char
import Data.Sequence (replicateA)
import Data.Ix (range)
import Data.Binary.IEEE754
import Codec.Compression.GZip
import Control.Exception
import Data.UnixTime
-- import Data.Time.Compat
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Maybe

import Debug.Trace


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
getAtr = Atr <$> getTime <*> mcn

getTime :: Get (Maybe UTCTime)
getTime = do
    secs <- u4
    return $ if secs == 0 then Nothing else Just $ posixSecondsToUTCTime $ realToFrac secs

getMilliseconds :: Get (Maybe Milliseconds)
getMilliseconds = do
  milliseconds <- mu4
  return $ Milliseconds <$> milliseconds

getMinutes :: Get (Maybe Minutes)
getMinutes = do
    mins <- u2
    return $ if mins == 65535 then Nothing else Just $ Minutes mins

getMir :: Get Rec
getMir = Mir <$> getTime <*> getTime <*> u1 <*> mc1<*> mc1  <*> mc1 <*> getMinutes
             <*> mc1 <*> cn <*> cn <*> cn <*> cn <*> cn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn

getMrr :: Get Rec
getMrr = Mrr <$> getTime <*> mc1 <*> mcn <*> mcn

getPcr :: Get Rec
getPcr = Pcr <$> u1 <*> u1 <*> u4 <*> mu4 <*> mu4 <*> mu4 <*> mu4 

getHbr :: Get Rec
getHbr = Hbr <$> u1 <*> u1 <*> u2 <*> u4 <*> getPassFailBin <*> mcn

getSbr :: Get Rec
getSbr = Sbr <$> u1 <*> u1 <*> u2 <*> u4 <*> getPassFailBin <*> mcn

getPmr :: Get Rec
getPmr = Pmr <$> u2 <*> mu2e0 <*> mcn <*> mcn <*> mcn <*> u1 <*> u1

getPgr :: Get Rec
getPgr = Pgr <$> u2 <*> mcn <*> getU2List

getPlr :: Get Rec
getPlr = do
    k <- fromIntegral <$> u2
    indecies <- replicateM k u2
    grpMode <- replicateM k u2
    let groupModes = map toGroupMode grpMode
    grpRdx <-  replicateM k u1
    let radixes = map toRadix grpRdx
    pgmChaR <- replicateM k mcn
    rtnChaR <- replicateM k mcn
    pgmChaL <- replicateM k mcn
    rtnChaL <- replicateM k mcn

    -- let pgmChars = zipWith (++) pgmChaL pgmChaR
    -- let rtnChars = zipWith (++) rtnChaL rtnChaR

    return $ Plr indecies groupModes radixes pgmChaR rtnChaR pgmChaL rtnChaL

toGroupMode :: U2 -> GroupMode
toGroupMode 00 = UnknownGroupMode
toGroupMode 10 = Normal
toGroupMode 20 = SameCycleIO
toGroupMode 21 = SameCycleMidband
toGroupMode 22 = SameCycleValid
toGroupMode 23 = SameCycleWindowSustain
toGroupMode 30 = DualDrive
toGroupMode 31 = DualDriveMidband
toGroupMode 32 = DualDriveValid
toGroupMode 33 = DualDriveWindowSustain
toGroupMode x = OtherGroupMode x

toRadix :: U1 -> Radix
toRadix 0  = DefaultRadix
toRadix 2  = Binary
toRadix 8  = Octal
toRadix 10 = Decimal
toRadix 16 = Hexadecimal
toRadix 20 = Symbolic
toRadix x  = OtherRadix x

getRdr :: Get Rec
getRdr = Rdr <$> getU2List

getSdr :: Get Rec
getSdr = Sdr <$> u1 <*> u1 <*> getU1List <*> mcn <*> mcn 
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
             <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn
-- getSdr = Sdr <$> u1 <*> u1 <*> u1 <*> getU1List <*> replicateA 16 mcn

getWir :: Get Rec
getWir = Wir <$> u1 <*> u1 <*> getTime <*> mcn

getWrr :: Get Rec
getWrr = Wrr <$> u1 <*> u1 <*> getTime <*> u4 <*> mu4 <*> mu4 <*> mu4
             <*> mu4 <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn <*> mcn

getWcr :: Get Rec
getWcr = Wcr <$> mr4 <*> mr4 <*> mr4 <*> getWaferUnits <*> getDirection 
             <*> mi2 <*> mi2 <*> getDirection <*> getDirection

getPassFailBin :: Get PassFailBin
getPassFailBin = do
    c <- c1
    return $ case c of
        'P' -> PassBin
        'F' -> FailBin
        ' ' -> UnknownBin
        _   -> OtherBin c

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
r4 = getFloat32le

r8 :: Get R8
r8 = getFloat64le

i1 :: Get I1
i1 = fromIntegral <$> u1

i2 :: Get I2
i2 = fromIntegral <$> u2

i4 :: Get I4
i4 = fromIntegral <$> u4

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

getPartFlag :: Get PartFlag
getPartFlag = do
    b <- u1
    return PartFlag { supersedesPartId = bit b 0
                   , supersedesXY     = bit b 1
                   , abnormalEnd      = bit b 2
                   , failed           = bit b 3
                   , noPassFailInfo   = bit b 4 }

getPrr :: Get Rec
getPrr = Prr <$> u1 <*> u1 <*> getPartFlag <*> u2 <*> u2 <*> mu2
             <*> mi2 <*> mi2 <*> getMilliseconds <*> mcn <*> mcn <*> mcn

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

decodeTestFlags :: U1 -> [TestFlag]
decodeTestFlags fl = if fl == 0
                        then [Pass]
                        else mkFlagList mapTf fl
                     where
                         mapTf 0 = Alarm
                         mapTf 1 = Invalid     -- bit 1
                         mapTf 2 = Unreliable  -- bit 2
                         mapTf 3 = Timeout     -- bit 3
                         mapTf 4 = NotExecuted -- bit 4
                         mapTf 5 = Aborted     -- bit 5
                         mapTf 6 = InValid     -- bit 6
                         mapTf 7 = Fail        -- bit 7

decodeParametricFlags :: U1 -> [ParametricFlag]
decodeParametricFlags = mkFlagList mapPf
    where
        mapPf 0 =  ScaleError -- I could probably get this from Enum duh
        mapPf 1 =  DriftError          -- bit 1
        mapPf 2 =  Oscillation         -- bit 2
        mapPf 3 =  FailHighLimit       -- bit 3
        mapPf 4 =  FailLowLimit        -- bit 4
        mapPf 5 =  PassAlternateLimits -- bit 5
        mapPf 6 =  PassOnEqLowLimit    -- bit 6
        mapPf 7 =  PassOnEqHighLimit   -- bit 7

mkFlagList :: (Int -> a) -> U1 -> [a]
mkFlagList func flags = [func bi | bi <- range (0, 7), testBit flags bi]

getDecodeFlags :: (U1 -> [a]) -> Get [a]
getDecodeFlags decodeF = do
    fl <- u1
    return $ decodeF fl

getTestFlags :: Get [TestFlag]
getTestFlags = getDecodeFlags decodeTestFlags

getParametricFlags :: Get [ParametricFlag]
getParametricFlags = getDecodeFlags decodeParametricFlags

getPtr :: Get Rec
getPtr = do
        ptrTestNum <- u4
        ptrHeadNum <- u1
        ptrSiteNum <- u1
        testFlags <- getTestFlags
        parametricFlags <- getParametricFlags
        result <- r4 -- depends on testFlags and parametric flags
        testText <- mcn
        alarmId <- mcn -- part of optionalInfo now
        -- optionalInfo <- getOptionalInfo
        let optionalInfo = [] -- TODO
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
                      optionalInfo
        where
            validResult tf pf = all (`elem` [Pass, Fail]) tf 
                             && all (`notElem` [ScaleError, DriftError, Oscillation]) pf
{-
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
-}

-- TODO: So Mpr doesn't use the Invalid flag
getMpr :: Get Rec
getMpr = do
    testNum <- u4
    headNum <- u1
    siteNum <- u1
    testFlg <- getTestFlags
    parmFlg <- getParametricFlags
    -- TODO: record may end here if no more info
    j <- fromIntegral <$> u2
    -- TODO: record may end here if no more info
    k <- fromIntegral <$> u2
    rtnStat <- getNibbles j :: Get [U1]
    rtnRslt <- replicateM k r4
    testTxt <- mcn
    -- TODO: record may end here if no more info
    let info = [] -- TODO: parse OptionalInfo

    return $ Mpr testNum headNum siteNum testFlg parmFlg rtnStat rtnRslt testTxt info

-- Take a list of bytes and split into exactly n nibbles
fromNibbles :: [U1] -> Int -> [U1]
fromNibbles [byte] 0         = []
fromNibbles [byte] 1         = [0xF .&. byte]
fromNibbles [byte] 2         = [0xF .&. byte, byte `shiftR` 4]
fromNibbles (byte:bytes) n = 0xF .&. byte : byte `shiftR` 4 : fromNibbles bytes (n - 2)

getNibbles :: Int -> Get [U1]
getNibbles nnibs = do
    let nbytes = nnibs `div` 2 + if odd nnibs then 1 else 0
    bytes <- replicateM nbytes u1
    return $ fromNibbles bytes nnibs


getOnFalse :: Bool -> Get a -> Get (Maybe a)
getOnFalse cond get = do
    x <- get
    return $ if cond
            then Nothing
            else Just x

getFtr :: Get Rec
getFtr = do
    testNum <- u4
    headNum <- u1
    siteNum <- u1
    testFlag <- getTestFlags  -- doesn't use the invalid bit
    -- record may end here, before rtnIcnt or pgmIcnt -- check isEmpty
    empty <- isEmpty
    info <- if empty then return []
            else getInfo
    -- may end here
    emptyj <- isEmpty
    j0 <- if emptyj then return 0
         else fromIntegral <$> u2
    -- may end here
    emptyk <- isEmpty
    k0 <- if emptyk then return 0
         else fromIntegral <$> u2

    emptymi <- isEmpty
    moreInfo <- if emptymi then return []
                else getMoreInfo j0 k0

    let allInfo = info ++ moreInfo

    return $ Ftr testNum headNum siteNum testFlag allInfo
                -- cycleCnt relVadr reptCnt numFail 
                -- xFail yFail vectOff rtnIndx rtnStat pgmIndx pgmStat
                -- failPin vecNam timeSet opCode testTxt alarmId progTxt rsltTxt patgNum spinMap

  where 
   getMoreInfo j k = do
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

    -- can it end here?
    patgNum <- mu1e255
    spinMap <- getBitField
    let patgNum = Nothing
    let spinMap = []

    let rtnIndx' = if j == 0 then Nothing else Just rtnIndx
    let rtnStat' = if j == 0 then Nothing else Just rtnStat
    let pgmIndx' = if k == 0 then Nothing else Just pgmIndx
    let pgmStat' = if k == 0 then Nothing else Just pgmStat
    let failPin' = if Prelude.null failPin then Nothing else Just failPin
    let spinMap' = if Prelude.null spinMap then Nothing else Just spinMap

    return $ catMaybes [ ReturnPinIndecies <$> rtnIndx'
                       , ReturnedStates <$> rtnStat'
                       , PgmStateIndecies <$> pgmIndx'
                       , PgmStates <$> pgmStat'
                       , FailPin <$> failPin'
                       , VectorName <$> vecNam
                       , TimeSet <$> timeSet
                       , OpCode <$> opCode
                       , Label <$> testTxt
                       , AlarmId <$> alarmId
                       , ProgramText <$> progTxt
                       , ResultText <$> rsltTxt
                       , PatternGen <$> patgNum
                       , EnabledPins <$> spinMap' ]

   getInfo = do
    optFlag <- u1

    let [noCycleCnt,
         noRelVadr,
         noReptCnt,
         noNumFail,
         noXYFail,
         noVectOff] = Prelude.map (testBit optFlag) $ range (0, 5)

    -- Turns out you're supposed to get the byte
    -- even if it's invalid
    -- For a format seemingly obsessed with saving
    -- a few bytes, this is surprising
    cycleCnt <- getOnFalse noCycleCnt u4
    relVadr  <- getOnFalse noRelVadr u4
    reptCnt  <- getOnFalse noReptCnt u4
    numFail  <- getOnFalse noNumFail u4
    xFail    <- getOnFalse noXYFail i4
    yFail    <- getOnFalse noXYFail i4
    vectOff  <- getOnFalse noVectOff i2

    return $ catMaybes
              [CycleCount <$> cycleCnt,
              RelativeVectorAddr <$> relVadr,
              RepeatCount <$> reptCnt,
              NumFailingPins <$> numFail,
              XLogicalFailureAddr <$> xFail,
              YLogicalFailureAddr <$> yFail,
              OffsetFromVector <$> vectOff]

-- TODO: BitField type like [U1] which toJson prints as hex "FF AF 12 or like STDFreader print bits 10000000 00010101
getBitField :: Get [U1]
getBitField = do
    nbits <- u2
    let nbytes = let (mbytes, mbits) = nbits `divMod` 8
                     obytes = mbytes + if mbits == 0 then 0 else 1
                 in fromIntegral obytes
    replicateM nbytes u1

getByteField :: Get [U1]
getByteField = do
    nbytes <- fromIntegral <$> u1
    replicateM nbytes u1

getBps :: Get Rec
getBps = Bps <$> mcn

getEps :: Get Rec
getEps = return Eps

getGdr :: Get Rec
getGdr = do
    fieldCount <- fromIntegral <$> u2
    fields <- replicateM fieldCount getGdrField
    return $ Gdr fields

getGdrField :: Get GdrField
getGdrField = do
    fieldType <- fromIntegral <$> u1
    case fieldType of
        0 -> return GPad
        1 -> GU1 <$> u1
        2 -> GU2 <$> u2
        3 -> GU4 <$> u4
        4 -> GI1 <$> i1
        5 -> GI2 <$> i2
        6 -> GI4 <$> i4
        7 -> GFloat <$> r4
        8 -> GDouble <$> r8
        10 -> GStr <$> cn
        11 -> GBytes <$> getByteField -- first 2 bytes are length in bytes
        12 -> GData <$> getBitField -- first 2 bytes are length in bits. I think I wrote something like this already
        13 -> GNibble <$> (liftM head . getNibbles) 1
        _ -> return GPad -- So don't crash on broken GDR

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
specificGet (Header _ 15 15) = getMpr
specificGet (Header _ 15 20) = getFtr
-- -- Per-Program-Segment
specificGet (Header _ 20 10) = getBps
specificGet (Header _ 20 20) = getEps
-- -- Generic Data
specificGet (Header _ 50 10) = getGdr
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

isGzipped :: ByteString -> IO Bool
isGzipped bs = do
    let magic = runGet getWord16le bs
    return $ magic == 0x8b1f

-- | Parse an optionally-gzipped stdf file
parseFile :: String -> IO Stdf
parseFile fn = do
    bs <- BL.readFile fn
    isgz <- isGzipped bs
    let decompressed = decompress bs
    return $ parse $ if isgz then decompressed else bs

-- | Parse an Stdf from a ByteString in case you want to open your own files or
-- | parse a stream off the tester or something
parse :: ByteString -> Stdf
parse = runGet getStdf

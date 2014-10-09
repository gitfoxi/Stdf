
{-# LANGUAGE DeriveGeneric #-}

module Data.Stdf.Types where

import Data.Word
import Data.Int
import Foreign.C.Types
import Data.Text.Lazy
import GHC.Generics hiding (U1, C1)
import Data.Aeson

type U1 = Word8  -- unsigned 1 byte
type U2 = Word16 -- unsigned 2 bytes
type U4 = Word32 -- unsigned 4 bytes
type I1 = Int8
type I2 = Int16
type I4 = Int32
type R4 = Float -- CFloat
type R8 = Double -- CDouble
type C1 = Char

instance ToJSON Rec
instance ToJSON PartFlg
instance ToJSON GdrField
instance ToJSON TestType
instance ToJSON WaferUnits
instance ToJSON Direction
instance ToJSON OptionalInfo

data BinRec = BinRec 
    { header :: Header
    , rec :: Rec } deriving (Generic, Show)

data Header = Header
    { len :: !Word16
    , typ :: !Word8
    , sub :: !Word8
    } deriving (Generic, Show)

type Stdf = [Rec]

-- The mother of all datatypes
data Rec= Raw { raw :: Text } -- base64 TODO: maybe don't bother. what's this good for?
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
        | Pir { pirHeadNum :: !U1
              , pirSiteNum :: !U1 }
        | Dtr { textDat :: Text }
        | Hbr { prrHeadNum :: !U1
              , prrSiteNum :: !U1
              , hbinNum :: !U2
              , hbinCnt :: !U4
              , hbinPassFail :: Maybe C1 -- Character, ' ' means Nothing
              , hbinName :: Maybe Text }
        | Wrr { wrrHeadNum :: !U1
              , siteGrp :: Maybe U1  -- 255 means Nothing
              , finishTime :: !U4
              , partCount :: !U4
              , retestCount :: Maybe U4 -- 4,294,967,295 -> Nothing
              , abortCount :: Maybe U4 -- 4,294,967,295 -> Nothing
              , goodCount :: Maybe U4 -- 4,294,967,295 -> Nothing
              , functionalCount :: Maybe U4 -- 4,294,967,295 -> Nothing
              , waferId :: Maybe Text -- length 0 -> Nothing
              , fabWaferId :: Maybe Text -- length 0 -> Nothing
              , waferFrameId :: Maybe Text -- length 0 -> Nothing
              , waferMaskId :: Maybe Text -- length 0 -> Nothing
              , userDescription :: Maybe Text -- length 0 -> Nothing
              , execDescription :: Maybe Text }
        | Mir { setupTime :: !U4
              , startTime :: !U4
              , stationNum :: !U1
              , modeCode :: Maybe C1 -- ' '
              , retestCode :: Maybe C1 -- ' '
              , protectionCode :: Maybe C1 -- ' '
              , burninTime :: Maybe U2 -- 65,535
              , commandMode :: Maybe C1 -- ' '
              , lotId :: Text
              , partType :: Text
              , nodeName :: Text
              , testerType :: Text
              , jobName :: Text
              , jobRevision :: Maybe Text
              , subLotId :: Maybe Text
              , operatorName :: Maybe Text
              , execType :: Maybe Text
              , execVersion :: Maybe Text
              , testCode :: Maybe Text
              , testTemperature :: Maybe Text
              , userText :: Maybe Text
              , auxFile :: Maybe Text
              , packageType :: Maybe Text
              , familyId :: Maybe Text
              , dateCode :: Maybe Text
              , facilityId :: Maybe Text
              , floorId :: Maybe Text
              , processId :: Maybe Text
              , operationFreq :: Maybe Text
              , specName :: Maybe Text
              , specVersion :: Maybe Text
              , flowId :: Maybe Text
              , setupId :: Maybe Text
              , designRev :: Maybe Text
              , engineeringLotId :: Maybe Text
              , romCodeId :: Maybe Text
              , testerSerialNum :: Maybe Text
              , supervisorName :: Maybe Text }
        | Atr { modificationTime :: !U4
              , commandLine :: Maybe Text }
        | Mrr { finishTime :: !U4
              , lotDisposition :: Maybe C1
              , userDescription :: Maybe Text
              , execDescription :: Maybe Text }
        | Pcr { pcrHeadNum :: !U1
              , pcrSiteNum :: !U1
              , partCount :: !U4
              , retestCount :: Maybe U4
              , abortCount :: Maybe U4
              , goodCount :: Maybe U4
              , functionalCount :: Maybe U4 }
        | Sbr { sbrHeadNum :: !U1
              , sbrSiteNum :: !U1
              , softBinNum :: !U2
              , softBinCount :: !U4
              , softBinPassFail :: Maybe C1
              , softBinName :: Maybe Text }
        | Pmr { pinIndex :: !U2
              , channelType :: Maybe U2
              , channelName :: Maybe Text
              , physicalName :: Maybe Text
              , logicalName :: Maybe Text
              , pmrHeadNum :: !U1
              , pmrSiteNum :: !U1 }
        | Pgr { groupIndex :: !U2
              , groupName :: Maybe Text
              -- , indexCount :: !U2 -- maybe irrelevant; just for parsing
              , pmrIndecies :: [U2] } -- list of Pmr instead of refering to indecies
        | Plr { -- groupCount :: !U2 -- parsing only
                groupIndecies :: [U2]
              , groupModes :: Maybe [U2]
              , groupRadixes :: Maybe [U1]
              , programStateChars :: Maybe [Text] -- combine CharR and CharL at parse?
              , returnStateChars :: Maybe [Text]
              , programStateCharsLeft :: Maybe [Text]
              , returnStateCharsLeft :: Maybe [Text] }
        | Rdr { -- numBins :: !U2  -- If 0 all bins are retested and retestBins is Nothing
                retestBins :: [U2] }
        | Sdr { sdrHeadNum :: !U1
              , sdrSiteGrp :: !U1
              -- , siteCount :: !U1
              , siteNums :: [U1]
              , handlerType :: Maybe Text
              , handlerId :: Maybe Text
              , probeCardType :: Maybe Text
              , probeCardId :: Maybe Text
              , loadBoardType :: Maybe Text
              , loadBoardId :: Maybe Text
              , dibType :: Maybe Text
              , dibId :: Maybe Text
              , cableType :: Maybe Text
              , cableId :: Maybe Text
              , contactorType :: Maybe Text
              , contactorId :: Maybe Text
              , laserType :: Maybe Text
              , laserId :: Maybe Text
              , extraType :: Maybe Text
              , extraId :: Maybe Text }
        | Wir { wirHeadNum :: !U1
              , wirSiteGrp :: Maybe U1 -- 255 -> Nothing
              , wirStartTime :: !U4
              , wirWaferId :: Maybe Text }
        | Wcr { waferSize :: Maybe R4 -- 0 -> Nothing
              , dieHeight :: Maybe R4
              , dieWidth :: Maybe R4
              , waferUnits :: Maybe WaferUnits
              , waferFlat :: Maybe Direction
              , centerX :: Maybe I2
              , centerY :: Maybe I2
              , positiveXdirection :: Maybe Direction
              , positiveYdirection :: Maybe Direction }
        | Tsr { tsrHeadNum :: !U1
              , tsrSiteNum :: !U1
              , tsrTestType :: Maybe TestType
              , tsrTestNum :: !U4
              , execCount :: Maybe U4
              , failCount :: Maybe U4
              , alarmCount :: Maybe U4
              , testName :: Maybe Text
              , sequencerName :: Maybe Text
              , testLabel :: Maybe Text
              -- , optionalFlags :: !U1 -- parsing optional if last field in record
              , testTimeAverage :: Maybe R4 -- optional fields based on optionalFlags
              , valueMin :: Maybe R4 -- may make these another record type
              , valueMax :: Maybe R4
              , valueSum :: Maybe R4
              , valueSumOfSquares :: Maybe R4 }
        | Ptr { ptrTestNum :: !U4
              , ptrHeadNum :: !U1
              , ptrSiteNum :: !U1
              , testFlags :: !U1 -- further parsing bits
              , parametricFlags :: !U1 -- further parsing bits
              , result :: Maybe R4
              , testText :: Maybe Text
              , alarmId :: Maybe Text
              , optionalInfo :: Maybe OptionalInfo }
        | Mpr { mprTestNum :: !U4
              , mprHeadNum :: !U1
              , mprSiteNum :: !U1
              , mprTestFlags :: !U1 -- further parsing bits
              , mprParametricFlags :: !U1 -- further parsing bits
              , stateCount :: U2
              , resultCount :: U2
              , states :: Maybe Text -- array of states? stateCount states
              , results :: Maybe [R4] -- resultCount results
              , mprTestText :: Maybe Text
              , mprAlarmId :: Maybe Text
              -- , OPT_FLG B1 optional stuff to parse
              , mprResultExp :: Maybe I1
              , mprLowLimitExp :: Maybe I1
              , mprHighLimitExp :: Maybe I1
              , mprLowTestLimit :: Maybe R4
              , mprHighTestLimit :: Maybe R4
              , startingInput :: Maybe R4
              , incrementInput :: Maybe R4
              , returnIndecies :: Maybe [U2]
              , mprUnits :: Maybe Text
              , mprPrintfResultFmt :: Maybe Text
              , mprPrintfLowLimitFmt :: Maybe Text
              , mprPrintfHighLimitFmt :: Maybe Text
              , mprLowSpecLimit :: Maybe R4
              , mprHighSpecLimit :: Maybe R4 }
        | Ftr { ftrTestNum :: !U4
              , ftrHeadNum :: !U1
              , ftrSiteNum :: !U1
              , ftrTestFlags :: !U1 -- 8 bit packed binary
              -- , optFlg :: !U1 -- 8 bit packed binary -- record may have ended by here
              , cycleCount :: Maybe U4
              , relativeVectorAddr :: Maybe U4
              , numFailingPins :: Maybe U4
              , xLogicalFailureAddr :: Maybe I4
              , yLogicalFailureAddr :: Maybe I4
              , offsetFromVector :: Maybe I2
              -- j U2
              -- k U2
              , pmrIndecies :: Maybe [U2] -- j x U2
              , returnedStates :: Maybe [U1] -- j NIBBLES!
              , pgmStateIndecies :: Maybe [U2] -- k x U2
              , pgmStates :: Maybe [U1] -- k NIBBLES!
              , failPin :: Maybe [U1] -- bitfield!
              , vectorName :: mcn
              , timeSetName :: mcn
              , opCode :: mcn
              , ftrText :: mcn
              , ftrAlarmId :: mcn
              , progText :: mcn
              , resultText :: mcn
              , patternGenNum :: Maybe U1  -- 255
              , enabledPins :: Maybe [U1] -- bitfield!
              } -- TODO: this is a long silly record. there's a bunch more things
        | Bps { sequencerName :: Maybe Text }  -- Begin Program Secion
        | Eps -- End Program Section: no payload
        | Gdr GdrField
          deriving (Generic, Show)

data OptionalInfo =
            OptionalInfo {
                resultExp :: Maybe I1
              , lowLimitExp :: Maybe I1
              , highLimitExp :: Maybe I1
              , lowTestLimit :: Maybe R4
              , highTestLimit :: Maybe R4
              , units :: Maybe Text
              , printfResultFmt :: Maybe Text
              , printfLowLimitFmt :: Maybe Text
              , printfHighLimitFmt :: Maybe Text
              , lowSpecLimit :: Maybe R4
              , highSpecLimit :: Maybe R4 } deriving (Generic, Show)

data TestType = Parametric
              | Functional
              | MultiResultParametric
              | UnknownTestType
              | OtherTestType C1
              deriving (Generic, Show)

data WaferUnits = Inches
                | Centimeters
                | Millimeters
                | Mils
                | OtherUnits U1
                deriving (Generic, Show)

data Direction = Up
               | Down
               | Left
               | Right
               | OtherDirection C1
               deriving (Generic, Show)

data GdrField = GPad -- discard
              | GU1 !U1
              | GU2 !U2
              | GU4 !U4
              | GI1 !I1
              | GI2 !I2
              | GI4 !I4
              | GFloat Float -- parse as CFloat
              | GDouble Double -- parse as CDouble
              | GStr Text
              | GBytes Text -- encoded ByteStr
              | GData Text -- 2byte length + encoded ByteStr
              | GNibble !U1 -- a nibble? are you fucking kidding me?
              deriving (Generic, Show)



data PartFlg = PartFlg { supersedesPartId :: Bool -- 1 bit
                       , supersedesXY :: Bool -- 1 bit
                       , abnormalEnd :: Bool
                       , failed :: Bool
                       , noPassFailInfo :: Bool }
                       deriving (Generic, Show)

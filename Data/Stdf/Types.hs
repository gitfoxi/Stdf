
{-# LANGUAGE DeriveGeneric #-}

module Data.Stdf.Types where

import Data.Word
import Data.Int
import Data.ByteString.Char8
import GHC.Generics hiding (U1, C1)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Text.Encoding as ByteString

-- Time is local unix time. Shouldn't they put the time zone in the file
-- in order for this to have any meaning? What if the data isn't being parsed
-- in the same time zone as it was generated? It will be as if the tester
-- was transported through time and space to wherever I am. I think I'll
-- just leave it alone so times can be Word32 representing Unix time

type U1 = Word8  -- unsigned 1 byte
type U2 = Word16 -- unsigned 2 bytes
type U4 = Word32 -- unsigned 4 bytes
type I1 = Int8
type I2 = Int16
type I4 = Int32
type R4 = Float -- CFloat
type R8 = Double -- CDouble
type C1 = Char

jsonOptions :: Options
jsonOptions = defaultOptions {
      allNullaryToStringTag = False
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    }

flagOptions :: Options
flagOptions = defaultOptions

-- warns of "Orphan instance" but without it, won't derive other ToJSON instances
-- could be much faster if text was made by copying the buffer rather than decoding
-- but decodeASCII is depricated
instance ToJSON ByteString where
    toJSON = toJSON . ByteString.decodeLatin1

instance ToJSON Milliseconds where
    toJSON = genericToJSON jsonOptions
instance ToJSON Minutes where
    toJSON = genericToJSON jsonOptions
instance ToJSON Rec where
    toJSON = genericToJSON jsonOptions
instance ToJSON PartFlag where
    toJSON = genericToJSON jsonOptions
instance ToJSON GdrField where
    toJSON = genericToJSON jsonOptions
instance ToJSON TestType where
    toJSON = genericToJSON jsonOptions
instance ToJSON WaferUnits where
    toJSON = genericToJSON jsonOptions
instance ToJSON Direction where
    toJSON = genericToJSON jsonOptions
instance ToJSON OptionalInfo where
    toJSON = genericToJSON jsonOptions
instance ToJSON GroupMode where
    toJSON = genericToJSON jsonOptions
instance ToJSON Radix where
    toJSON = genericToJSON jsonOptions
instance ToJSON TestFlag where
    toJSON = genericToJSON flagOptions
instance ToJSON PassFailBin where
    toJSON = genericToJSON jsonOptions
instance ToJSON ParametricFlag where
    toJSON = genericToJSON flagOptions

data BinRec = BinRec 
    { header :: Header
    , rec :: Rec } deriving (Generic, Show)

data Header = Header
    { len :: !Word16
    , typ :: !Word8
    , sub :: !Word8
    } deriving (Generic, Show)

type Stdf = [Rec]

data Milliseconds = Milliseconds { ms :: !U4 }
    deriving (Generic, Show)

data Minutes = Minutes { minutes :: !U2 }
    deriving (Generic, Show)

-- The mother of all datatypes
data Rec =
      Raw { raw :: !ByteString } -- base64 TODO: URL encoding or maybe don't bother. what's this good for?
    | Far { cpuType  :: !U1
          , stdfVersion  :: !U1 }
    | Atr { modificationTime :: !(Maybe UTCTime)
          , commandLine :: Maybe ByteString }
    | Mir { setupTime :: Maybe UTCTime
          , startTime :: Maybe UTCTime
          , station :: !U1
          , modeCode :: Maybe C1 -- TODO: MODE_COD record
          , retestCode :: Maybe C1 -- TODO: RTST_COD record
          , protectionCode :: Maybe C1 -- ' '
          , burninTime :: Maybe Minutes -- 65,535
          , commandCode :: Maybe C1 -- ' '
          , lotId :: ByteString
          , partType :: ByteString
          , nodeName :: ByteString
          , testerType :: ByteString
          , jobName :: ByteString
          , jobRevision :: Maybe ByteString
          , subLotId :: Maybe ByteString
          , operatorName :: Maybe ByteString
          , execType :: Maybe ByteString
          , execVersion :: Maybe ByteString
          , testCode :: Maybe ByteString
          , testTemperature :: Maybe ByteString
          , userByteString :: Maybe ByteString
          , auxFile :: Maybe ByteString
          , packageType :: Maybe ByteString
          , familyId :: Maybe ByteString
          , dateCode :: Maybe ByteString
          , facilityId :: Maybe ByteString
          , floorId :: Maybe ByteString
          , processId :: Maybe ByteString
          , operationFreq :: Maybe ByteString
          , specName :: Maybe ByteString
          , specVersion :: Maybe ByteString
          , flowId :: Maybe ByteString
          , setupId :: Maybe ByteString
          , designRev :: Maybe ByteString
          , engineeringLotId :: Maybe ByteString
          , romCodeId :: Maybe ByteString
          , testerSerialNum :: Maybe ByteString
          , supervisorName :: Maybe ByteString }
    | Mrr { finishTime :: Maybe UTCTime
          , lotDisposition :: Maybe C1
          , userDescription :: Maybe ByteString
          , execDescription :: Maybe ByteString }
    | Pcr { headId :: !U1
          , siteId :: !U1
          , partCount :: !U4
          , retestCount :: Maybe U4
          , abortCount :: Maybe U4
          , goodCount :: Maybe U4
          , functionalCount :: Maybe U4 }
    | Hbr { headId :: !U1
          , siteId :: !U1
          , bin :: !U2
          , binCount :: !U4
          , passFailBin :: PassFailBin
          , name :: Maybe ByteString }
    | Sbr { headId :: !U1
          , siteId :: !U1
          , bin :: !U2
          , binCount :: !U4
          , passFail :: PassFailBin
          , name :: Maybe ByteString }
    | Pmr { index :: !U2 -- Maybe rather than index reference by name
          , channelType :: Maybe U2
          , channelName :: Maybe ByteString
          , physicalName :: Maybe ByteString
          , logicalName :: Maybe ByteString
          , headId :: !U1
          , siteId :: !U1 }
    | Pgr { index :: !U2
          , name :: Maybe ByteString
          , pinIndecies :: [U2] } -- list of pins instead of refering to indecies
    -- Parsing: 
    -- Empty arrays (or empty members of arrays) can be omitted 
    -- if they occur at the end of the record.
    | Plr { 
          -- Instead of Maybe [] opt for empty [Maybe]
            indecies :: [U2]
          , groupModes :: [GroupMode]
          , groupRadixes :: [Radix]
          -- Should really just use a string for state characters
          -- instead of Left/Right but failed since I don't have example PLR
          -- , programStateChars :: [ByteString] -- combine CharR and CharL at parse?
          -- , returnStateChars :: [ByteString] }
          , programStateCharsRight :: [Maybe ByteString]
          , returnStateCharsRight :: [Maybe ByteString]
          , programStateCharsLeft :: [Maybe ByteString]
          , returnStateCharsLeft :: [Maybe ByteString] }
    | Rdr { retestBins :: [U2] }
    | Sdr { headId :: !U1
          , siteGroup :: !U1
          , sites :: [U1]
          , handlerType :: Maybe ByteString
          , handlerId :: Maybe ByteString
          , probeCardType :: Maybe ByteString
          , probeCardId :: Maybe ByteString
          , loadBoardType :: Maybe ByteString
          , loadBoardId :: Maybe ByteString
          , dibType :: Maybe ByteString
          , dibId :: Maybe ByteString
          , cableType :: Maybe ByteString
          , cableId :: Maybe ByteString
          , contactorType :: Maybe ByteString
          , contactorId :: Maybe ByteString
          , laserType :: Maybe ByteString
          , laserId :: Maybe ByteString
          , extraType :: Maybe ByteString
          , extraId :: Maybe ByteString }
    | Wir { headId :: !U1
          , siteGroup :: !U1 -- 255 -> Nothing -- feature removed
          , startTime :: Maybe UTCTime
          , waferId :: Maybe ByteString }
    | Wrr { headId :: !U1
          , siteGroup :: !U1  -- 255 means Nothing
          , finishTime :: Maybe UTCTime
          , partCount :: !U4
          , retestCount :: Maybe U4 -- 4,294,967,295 -> Nothing
          , abortCount :: Maybe U4 -- 4,294,967,295 -> Nothing
          , goodCount :: Maybe U4 -- 4,294,967,295 -> Nothing
          , functionalCount :: Maybe U4 -- 4,294,967,295 -> Nothing
          , waferId :: Maybe ByteString -- length 0 -> Nothing
          , fabWaferId :: Maybe ByteString -- length 0 -> Nothing
          , waferFrameId :: Maybe ByteString -- length 0 -> Nothing
          , waferMaskId :: Maybe ByteString -- length 0 -> Nothing
          , userDescription :: Maybe ByteString -- length 0 -> Nothing
          , execDescription :: Maybe ByteString }
    | Wcr { waferSize :: Maybe R4 -- 0 -> Nothing
          , dieHeight :: Maybe R4
          , dieWidth :: Maybe R4
          , waferUnits :: Maybe WaferUnits
          , waferFlat :: Maybe Direction
          , centerX :: Maybe I2
          , centerY :: Maybe I2
          , positiveXdirection :: Maybe Direction
          , positiveYdirection :: Maybe Direction }
    | Pir { headId :: !U1
          , siteId :: !U1 }
    | Prr { headId  :: !U1
            , siteId  :: !U1
            , partFlag  :: !PartFlag
            , numTestsExecuted  :: !U2
            , hardBin  :: !U2
            , softBin  :: Maybe U2
            , xCoord   :: Maybe I2
            , yCoord   :: Maybe I2
            , testTime :: Maybe Milliseconds
            , partID   :: Maybe ByteString
            , partTxt  :: Maybe ByteString
            , partFix  :: Maybe ByteString }
    | Tsr { headId :: !U1
          , siteId :: !U1
          -- TODO: Json printing testType wierd like
          -- "testType":{"MultiResultParametric":[]}
          , testType :: TestType 
          , testId :: !U4
          , execCount :: Maybe U4
          , failCount :: Maybe U4
          , alarmCount :: Maybe U4
          , testName :: Maybe ByteString
          , sequencerName :: Maybe ByteString
          , testLabel :: Maybe ByteString
          , averageSeconds :: Maybe R4
          , valueMin :: Maybe R4 -- may make these another record type like stats
          , valueMax :: Maybe R4
          , valueSum :: Maybe R4
          , valueSumOfSquares :: Maybe R4 }
    | Ptr { testId :: !U4
          , headId :: !U1
          , siteId :: !U1
          -- TODO: testFlags would be more convenient as a Set than a List
          , testFlags :: [TestFlag] -- B1 bitfield further parsing bits
          , parametricFlags :: [ParametricFlag] -- B1 bitfield further parsing bits
          , result :: Maybe R4
          , testByteString :: Maybe ByteString
          -- , alarmId :: Maybe ByteString -> optionalInfo
          , info :: Maybe [OptionalInfo] } -- TODO: better name
    | Mpr { testId :: !U4
          , headId :: !U1
          , siteId :: !U1
          , testFlags :: [TestFlag]
          , parametricFlags :: [ParametricFlag]
          -- j , stateCount :: U2
          -- k , resultCount :: U2
          -- , states :: [U1] -- Nibbles! array of states? j states
          -- , results :: [R4] -- k results
          -- , testByteString :: Maybe ByteString
          , info :: Maybe [OptionalInfo] }
          -- , alarmId :: Maybe ByteString
          -- -- , OPT_FLG B1 optional stuff to parse
          -- , resultExp :: Maybe I1
          -- , lowLimitExp :: Maybe I1
          -- , highLimitExp :: Maybe I1
          -- , lowLimit :: Maybe R4
          -- , highLimit :: Maybe R4
          -- , startingInput :: Maybe R4
          -- , incrementInput :: Maybe R4
          -- , returnPinIndecies :: Maybe [U2] -- k indecies
          -- , units :: Maybe ByteString
          -- , unitsInputCondition  :: Maybe ByteString
          -- , printfResultFmt :: Maybe ByteString
          -- , printfLowLimitFmt :: Maybe ByteString
          -- , printfHighLimitFmt :: Maybe ByteString
          -- , lowSpecLimit :: Maybe R4
          -- , highSpecLimit :: Maybe R4 }
    | Ftr { testId :: !U4
          , headId :: !U1
          , siteId :: !U1
          , testFlags :: [TestFlag]
          -- , info :: Maybe [OptionalInfo]
          -- -- , optFlg :: !U1 -- 8 bit packed binary -- record may have ended by here
          , cycleCount :: Maybe U4    -- To Optional Info
          , relativeVectorAddr :: Maybe U4
          , repeatCount :: !(Maybe U4)
          , numFailingPins :: Maybe U4
          , xLogicalFailureAddr :: Maybe I4
          , yLogicalFailureAddr :: Maybe I4
          , offsetFromVector :: Maybe I2
          -- j U2
          -- k U2
          , pinIndexes :: Maybe [U2] -- j x U2
          , returnedStates :: Maybe [U1] -- j NIBBLES!
          , pgmStateIndecies :: Maybe [U2] -- k x U2
          , pgmStates :: Maybe [U1] -- k NIBBLES!
          , failPin :: Maybe [U1] -- bitfield!
          , vector :: Maybe ByteString
          , timeSet :: Maybe ByteString
          , opCode :: Maybe ByteString
          , testByteString :: Maybe ByteString
          , alarmId :: Maybe ByteString
          , programByteString :: Maybe ByteString
          , resultByteString :: Maybe ByteString
          , patternGen :: Maybe U1  -- 255
          , enabledPins :: Maybe [U1] -- bitfield!
          }
    | Bps { sequencerName :: Maybe ByteString }  -- Begin Program Secion
    | Eps -- End Program Section: no payload
    | Gdr [GdrField]
    | Dtr { textDat :: ByteString }
      deriving (Generic, Show)

data GroupMode = UnknownGroupMode
               | Normal
               | SameCycleIO
               | SameCycleMidband
               | SameCycleValid
               | SameCycleWindowSustain
               | DualDrive
               | DualDriveMidband
               | DualDriveValid
               | DualDriveWindowSustain
               | OtherGroupMode U2
               deriving (Generic, Show)

data Radix = DefaultRadix
           | Binary
           | Octal
           | Decimal
           | Hexadecimal
           | Symbolic
           | OtherRadix U1
           deriving (Generic, Show)

data TestFlag = Alarm       -- bit 0
              | Invalid     -- bit 1
              | Unreliable  -- bit 2
              | Timeout     -- bit 3
              | NotExecuted -- bit 4
              | Aborted     -- bit 5
              | InValid     -- bit 6
              | Pass        -- bit 7 == 0
              | Fail        -- bit 7
              deriving (Generic, Show, Eq, Enum)

data PassFailBin = PassBin | FailBin | UnknownBin | OtherBin Char
    deriving (Generic, Show)

data ParametricFlag = ScaleError          -- bit 0
                    | DriftError          -- bit 1
                    | Oscillation         -- bit 2
                    | FailHighLimit       -- bit 3
                    | FailLowLimit        -- bit 4
                    | PassAlternateLimits -- bit 5
                    | PassOnEqLowLimit    -- bit 6
                    | PassOnEqHighLimit   -- bit 7
                    -- bits 6 & 7 seem stupid
                    deriving (Generic, Show, Eq, Enum)

-- TODO: Another pass at scaling flags
-- Maybe better as sum type
data OptionalInfo   = Units ByteString
                    | LowSpecLimit R4
                    | HighSpecLimit R4
                    | LowSpecLimitStr ByteString
                    | HighSpecLimitStr ByteString
                    | AlarmId ByteString
                    | LowLimit R4
                    | HighLimit R4
                    | LowLimitStr ByteString
                    | HighLimitStr ByteString
                    | ResultStr ByteString
                    | StartingInput R4
                    | StartingInputUnits ByteString
                    | IncrementInput R4
                    -- change to a map of pinName -> state
                    | ReturnPinIndecies [U2]  -- k
                    | CycleCount U4
                    | RepeatCount U4
                    | RelativeVectorAddr U4
                    | NumFailingPins U4
                    | XLogicalFailureAddr I4
                    | YLogicalFailureAddr I4
                    | OffsetFromVector I2
                    | PinIndecies [U2] -- j x U2 -- redundant with ReturnPinIndecies?
                    | ReturnedStates [U1] -- j NIBBLES!
                    | PgmStateIndecies [U2] -- k x U2 -> Parse pin states?
                    | PgmStates [U1] -- k NIBBLES! -> String
                    | FailPin [U1] -- bitfield! -> [PinName]
                    | VectorName ByteString
                    | TimeSet ByteString
                    | OpCode ByteString
                    | Label ByteString
                    | ProgramByteString ByteString
                    | ResultByteString ByteString
                    | PatternGen U1  -- 255
                    | EnabledPins [U1] -- bitfield!
                    --
                    | Results [R4]
                    | ResultExp I1
                    | LowLimitExp I1
                    | HighLimitExp I1
                    | CResultFormat ByteString
                    | CLowLimitFormat ByteString
                    | CHighLimitFormat ByteString
                    deriving (Generic, Show, Eq)

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
              | GStr ByteString
              | GBytes [U1] -- encoded ByteStr
              | GData [U1] -- 2byte length + encoded ByteStr
              | GNibble !U1 -- a nibble? are you fucking kidding me?
              deriving (Generic, Show)

data PartFlag = PartFlag { supersedesPartId :: Bool -- 1 bit
                       , supersedesXY :: Bool -- 1 bit
                       , abnormalEnd :: Bool
                       , failed :: Bool
                       , noPassFailInfo :: Bool }
                       deriving (Generic, Show)

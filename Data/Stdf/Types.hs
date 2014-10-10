
{-# LANGUAGE DeriveGeneric #-}

module Data.Stdf.Types where

import Data.Word
import Data.Int
import Foreign.C.Types
import Data.Text.Lazy
import GHC.Generics hiding (U1, C1)
import Data.Aeson
import Data.Time.LocalTime

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

instance ToJSON Rec
instance ToJSON PartFlag
instance ToJSON GdrField
instance ToJSON TestType
instance ToJSON WaferUnits
instance ToJSON Direction
instance ToJSON OptionalInfo
instance ToJSON GroupMode
instance ToJSON Radix
instance ToJSON TestFlag
instance ToJSON PassFailBin
instance ToJSON ParametricFlag

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
data Rec= Raw { raw :: Text } -- base64 TODO: URL encoding or maybe don't bother. what's this good for?
        | Far { cpuType  :: !U1
              , stdfVersion  :: !U1 }
        | Atr { modificationTime :: !U4
              , commandLine :: Maybe Text }
        | Mir { setupTime :: !U4
              , startTime :: !U4
              , station :: !U1
              , modeCode :: Maybe C1 -- TODO: MODE_COD record
              , retestCode :: Maybe C1 -- TODO: RTST_COD record
              , protectionCode :: Maybe C1 -- ' '
              , burninTime :: Maybe U2 -- 65,535
              , commandCode :: Maybe C1 -- ' '
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
        | Mrr { finishTime :: !U4
              , lotDisposition :: Maybe C1
              , userDescription :: Maybe Text
              , execDescription :: Maybe Text }
        | Pcr { head :: !U1
              , site :: !U1
              , partCount :: !U4
              , retestCount :: Maybe U4
              , abortCount :: Maybe U4
              , goodCount :: Maybe U4
              , functionalCount :: Maybe U4 }
        | Hbr { head :: !U1
              , site :: !U1
              , bin :: !U2
              , binCount :: !U4
              , passFailBin :: PassFailBin -- TODO: HBIN_PF
              , name :: Maybe Text }
        | Sbr { head :: !U1
              , site :: !U1
              , bin :: !U2
              , binCount :: !U4
              , passFail :: PassFailBin
              , name :: Maybe Text }
        | Pmr { index :: !U2 -- Maybe rather than index reference by name
              , channelType :: Maybe U2
              , channelName :: Maybe Text
              , physicalName :: Maybe Text
              , logicalName :: Maybe Text
              , head :: !U1
              , site :: !U1 }
        | Pgr { index :: !U2
              , name :: Maybe Text
              , pinIndecies :: [U2] } -- list of pins instead of refering to indecies
        -- Parsing: 
        -- Empty arrays (or empty members of arrays) can be omitted 
        -- if they occur at the end of the record.
        | Plr { groupIndecies :: [U2]
              -- Instead of Maybe [] opt for empty [Maybe]
              , groupModes :: [GroupMode] -- TODO: GroupMode
              , groupRadixes :: [Radix] -- TODO: GroupRadix
              -- Should really just use a string for state characters
              -- instead of Left/Right
              , programStateChars :: [Text] -- combine CharR and CharL at parse?
              , returnStateChars :: [Text] }
              -- , programStateCharsLeft :: [Maybe Char]
              -- , returnStateCharsLeft :: [Maybe Char] }
        | Rdr { retestBins :: [U2] }
        | Sdr { head :: !U1
              , siteGroup :: !U1
              , sites :: [U1]
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
        | Wir { head :: !U1
              , siteGroup :: !U1 -- 255 -> Nothing -- feature removed
              , startTime :: !U4
              , waferId :: Maybe Text }
        | Wrr { head :: !U1
              , siteGroup :: !U1  -- 255 means Nothing
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
        | Wcr { waferSize :: Maybe R4 -- 0 -> Nothing
              , dieHeight :: Maybe R4
              , dieWidth :: Maybe R4
              , waferUnits :: Maybe WaferUnits
              , waferFlat :: Maybe Direction
              , centerX :: Maybe I2
              , centerY :: Maybe I2
              , positiveXdirection :: Maybe Direction
              , positiveYdirection :: Maybe Direction }
        | Pir { head :: !U1
              , site :: !U1 }
        | Prr { head  :: !U1
                , site  :: !U1
                , partFlag  :: !PartFlag
                , numTestsExecuted  :: !U2
                , hardBin  :: !U2
                , softBin  :: Maybe U2
                , xCoord   :: Maybe I2
                , yCoord   :: Maybe I2
                , testTime :: Maybe U4
                , partID   :: Maybe Text
                , partTxt  :: Maybe Text
                , partFix  :: Maybe Text }
        | Tsr { head :: !U1
              , site :: !U1
              , testType :: Maybe TestType
              , test :: !U4
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
        | Ptr { test :: !U4
              , head :: !U1
              , site :: !U1
              , testFlags :: [TestFlag] -- B1 bitfield further parsing bits
              , parametricFlags :: [ParametricFlag] -- B1 bitfield further parsing bits
              , result :: Maybe R4
              , testText :: Maybe Text
              -- , alarmId :: Maybe Text -> optionalInfo
              , info :: [OptionalInfo] } -- TODO: better name
        | Mpr { test :: !U4
              , head :: !U1
              , site :: !U1
              , testFlags :: [TestFlag]
              , parametricFlags :: [ParametricFlag]
              -- j , stateCount :: U2
              -- k , resultCount :: U2
              , states :: [Text] -- Nibbles! array of states? j states
              , results :: [R4] -- k results
              , testText :: Maybe Text
              , info :: [OptionalInfo] }
              -- , alarmId :: Maybe Text
              -- -- , OPT_FLG B1 optional stuff to parse
              -- , resultExp :: Maybe I1  -- TODO: put mostly in OptionalInfo
              -- , lowLimitExp :: Maybe I1
              -- , highLimitExp :: Maybe I1
              -- , lowLimit :: Maybe R4
              -- , highLimit :: Maybe R4
              -- , startingInput :: Maybe R4
              -- , incrementInput :: Maybe R4
              -- , returnPinIndecies :: Maybe [U2] -- k indecies
              -- , units :: Maybe Text
              -- , unitsInputCondition  :: Maybe Text
              -- , printfResultFmt :: Maybe Text
              -- , printfLowLimitFmt :: Maybe Text
              -- , printfHighLimitFmt :: Maybe Text
              -- , lowSpecLimit :: Maybe R4
              -- , highSpecLimit :: Maybe R4 }
        | Ftr { test :: !U4
              , head :: !U1
              , site :: !U1
              , testFlags :: [TestFlag]
              , info :: [OptionalInfo]
              -- -- , optFlg :: !U1 -- 8 bit packed binary -- record may have ended by here
              -- , cycleCount :: Maybe U4    -- To Optional Info
              -- , relativeVectorAddr :: Maybe U4
              -- , numFailingPins :: Maybe U4
              -- , xLogicalFailureAddr :: Maybe I4
              -- , yLogicalFailureAddr :: Maybe I4
              -- , offsetFromVector :: Maybe I2
              -- -- j U2
              -- -- k U2
              -- , pinIndecies :: Maybe [U2] -- j x U2
              -- , returnedStates :: Maybe [U1] -- j NIBBLES!
              -- , pgmStateIndecies :: Maybe [U2] -- k x U2
              -- , pgmStates :: Maybe [U1] -- k NIBBLES!
              -- , failPin :: Maybe [U1] -- bitfield!
              -- , vector :: Maybe Text
              -- , timeSet :: Maybe Text
              -- , opCode :: Maybe Text
              -- , label :: Maybe Text
              -- , alarmId :: Maybe Text
              -- , programText :: Maybe Text
              -- , resultText :: Maybe Text
              -- , patternGen :: Maybe U1  -- 255
              -- , enabledPins :: Maybe [U1] -- bitfield!
              } -- TODO: this is a long silly record. there's a bunch more things
        | Bps { sequencerName :: Maybe Text }  -- Begin Program Secion
        | Eps -- End Program Section: no payload
        | Gdr GdrField
        | Dtr { textDat :: Text }
          deriving (Generic, Show)

data GroupMode = UnknownGroupMode
               | Normal
               | SameCycleIO
               | SameCycleMidband
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

data TestFlag = Alarm 
              | Invalid
              | Unreliable
              | Timeout
              | NotExecuted
              | Aborted
              | Pass
              | Fail
              deriving (Generic, Show)

data PassFailBin = PassBin | FailBin | UnknownBin | OtherBin Char
    deriving (Generic, Show)

data ParametricFlag = ScaleError
                    | DriftError
                    | Oscillation
                    | FailHighLimit
                    | FailLowLimit
                    | PassAlternateLimits
                    -- Not bothering with bits 6 & 7 because stupid
                    deriving (Generic, Show)

-- TODO: Another pass at scaling flags
-- Maybe better as sum type
data OptionalInfo   = Units Text
                    | LowSpecLimit Float
                    | HighSpecLimit Float
                    | LowSpecLimitStr Text
                    | HighSpecLimitStr Text
                    | AlarmId Text
                    | LowLimit Float
                    | HighLimit Float
                    | LowLimitStr Text
                    | HighLimitStr Text
                    | ResultStr Text
                    | StartingInput Float
                    | StartingInputUnits Text
                    | IncrementInput Float
                    -- change to a map of pinName -> state
                    | ReturnPinIndecies [U2]  -- k
                    | CycleCount U4    -- To Optional Info
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
                    | Vector Text
                    | TimeSet Text
                    | OpCode Text
                    | Label Text
                    | ProgramText Text
                    | ResultText Text
                    | PatternGen U1  -- 255
                    | EnabledPins [U1] -- bitfield!
                    deriving (Generic, Show)

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

data PartFlag = PartFlag { supersedesPartId :: Bool -- 1 bit
                       , supersedesXY :: Bool -- 1 bit
                       , abnormalEnd :: Bool
                       , failed :: Bool
                       , noPassFailInfo :: Bool }
                       deriving (Generic, Show)

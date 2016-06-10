
module Data.Stdf.DataLog where

import Data.YAML

import qualified Data.Stdf.Types as Stdf

-- | Simplified datalog as a heirarhicy
--
-- Typical heirarcy:
--
--  DataLog
--    - Lot
--      - Wafer
--        - Part
--          - TestExec
--          - TestExec
--          - ...
--        - Part
--        - ...
--      - Wafer
--      - ...
--    - Lot
--    - ...

type Name = ByteString

data DataLog =
 DataLog
  { -- Top-level; combine Far, Atr, Tsr
  , lots :: [Lot]
  }

data Lot =
 Lot
  { -- , Mir, Mrr, Pcr, Hbr, Sbr, Pmr, Pgr, Plr, Rdr, Sdr
  -- Mir
  , jobRevision :: Name --  /home/caviumps/workorder/MAT_CN88XXYB_FT_FQA_REV04A_HAND
  , retestCode :: '0'
  , setupTime :: UTCTime --  2016-05-25T03:19:22.000000000000Z
  , lotId :: Name --  B13UCV6032.000.03_FT1_75C
  , startTime :: UTCTime --  2016-05-25T03:20:13.000000000000Z
  , jobName :: Name --  FT_FQA_CN88XXYB_REV04A_hand on device /home/caviumps/CN88XXYB_2601BG_REV04A/device
  , testCode :: Name --  FT1_T1
  , familyId :: Name --  CN88XXYB
  , processId :: Name --  CN88XXYB
  , execVersion :: Name --  ! 's/w rev. 7.1.4.7 (E), 18-Mar-13
  , flowId :: Name --  FT1_T1
  , nodeName :: Name --  ps5-64
  , operatorName :: Name --  CAVM
  , execType :: Name --  '93000'
  , testerType :: Name --  93000-SOC
  -- Mrr
  , finishTime :: UTCTime
  -- Sbr
  , softBinMap :: HashMap Int Name
  , allSoftBins :: [Name]
  -- Pmr
  , pinMap :: HashMap Int Name
  , allPins :: [Name]
  -- Sdr
  , loadBoardType: Name -- CN88XX_TYPE1
  -- Wir .. Wrr
  , wafers :: [Wafer]
  }

data Wafer =
   Wafer
    { -- Wir, Wrr, Wcr
    -- Wir
      waferId :: Maybe Name --  '01' -- none if package test
    , parts :: [Part] -- parts under Lot if wafer sort
    }

data Part =
 Part
  { -- Pir, Prr, Gdr, Dtr
    hardBin   :: Int
  , softBin   :: Name
  , testTimeMs:: Int
  , testExecs :: [TestExec]
  , messages :: [Object] -- YAML-formatted Dtr.textDat
  }

data TestExec
 TestExec
  { -- Ptr, Mpr, Ftr
    name :: ByteString
    passFail :: PassFail
  }
 ParametricTest
  {
    name :: ByteString
    passFail :: PassFail
  }
 MultiParametricTest
  {
    name :: ByteString
    passFail :: PassFail
  }

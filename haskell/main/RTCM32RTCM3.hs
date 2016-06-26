{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module:      RTCM32RTCM3
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCM3 to RTCM3 tool.
--
-- Used to replace fields of incoming RTCM messages. Example usage:
--
-- curl localhost:2101
--   | rtcm32rtcm3 --newPosX -2709557.185 --newPosY -4260015.645 --newPosZ 3884773.356
--   | rtcm32json | jq .

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary
import           Data.RTCM3
import           Options.Generic
import           System.IO

data Args = Args
  { newPosX   :: Maybe Double
  , newPosY   :: Maybe Double
  , newPosZ   :: Maybe Double
  } deriving ( Eq, Show, Generic )

instance ParseRecord Args

type PositionECEF = [Double]

argsToPos :: Args -> Maybe PositionECEF
argsToPos Args{..} = mapM id [newPosX, newPosY, newPosZ]

toEcefVal :: Double -> Int64
toEcefVal x = round $ x * 10000

newMsg1005 :: Maybe PositionECEF -> Msg1005 -> Msg1005
newMsg1005 Nothing  m         = m
newMsg1005 (Just pos) m       = Msg1005
  { _msg1005_reference = (m ^. msg1005_reference)
                           & antennaReference_ecef_x .~ toEcefVal (pos !! 0)
                           & antennaReference_ecef_y .~ toEcefVal (pos !! 1)
                           & antennaReference_ecef_z .~ toEcefVal (pos !! 2)
  }

newMsg1006 :: Maybe PositionECEF -> Msg1006 -> Msg1006
newMsg1006 Nothing  m         = m
newMsg1006 (Just pos) m       = Msg1006
  { _msg1006_reference = (m ^. msg1006_reference)
                           & antennaReference_ecef_x .~ toEcefVal (pos !! 0)
                           & antennaReference_ecef_y .~ toEcefVal (pos !! 1)
                           & antennaReference_ecef_z .~ toEcefVal (pos !! 2)
  , _msg1006_extReference = m ^. msg1006_extReference
  }

replace :: Args -> RTCM3Msg -> RTCM3Msg
replace args (RTCM3Msg1005 m _rtcm3) = RTCM3Msg1005 new $ toRTCM3 new
  where
    new = newMsg1005 (argsToPos args) m
replace args (RTCM3Msg1006 m _rtcm3) = RTCM3Msg1006 new $ toRTCM3 new
  where
    new = newMsg1006 (argsToPos args) m
replace _ _rtcm3Msg                  = _rtcm3Msg

main :: IO ()
main = do
  args <- getRecord "RTCM proxy"
  BasicPrelude.print (args :: Args)
  runResourceT            $
    sourceHandle stdin    =$=
    conduitDecode         =$=
    CL.map (replace args) =$=
    conduitEncode         $$
    sinkHandle stdout

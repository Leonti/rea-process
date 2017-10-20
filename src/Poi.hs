module Poi where

import Data.List.Split
import Data.List
import Data.Maybe

data StoreType = Woolworths | Coles | Aldi deriving (Show, Eq)

data Store = Store
    { storeType :: StoreType
    , storeLongitude :: Double
    , storeLatitude :: Double
    } deriving (Show)

storeTypeMap :: [(String, StoreType)]
storeTypeMap = [("WOOLWORTHS", Woolworths), ("COLES", Coles), ("ALDI", Aldi)]

-- 0 1 2 3 4
data StopType = Train | Tram | Bus | VLine | NightBus deriving (Show, Eq)

stopTypeMap :: [(String, StopType)]
stopTypeMap = [("0", Train), ("1", Tram), ("2", Bus), ("3", VLine), ("4", NightBus)]

data Stop = Stop
    { stopType :: StopType
    , stopLongitude :: Double
    , stopLatitude :: Double
    } deriving (Show)

lineToStop :: String -> Stop
lineToStop line =
    Stop sType lng lat
    where
        parts = splitOn "," line
        sType = fromJust $ lookup (parts!!0) stopTypeMap
        lng = read (parts!!1) :: Double
        lat = read  (parts!!2) :: Double

lineToStore :: String -> Store
lineToStore line =
    Store sType lng lat
    where
        parts = splitOn "," line
        sType = fromJust $ lookup (parts!!0) storeTypeMap
        lng = read (parts!!1) :: Double
        lat = read  (parts!!2) :: Double

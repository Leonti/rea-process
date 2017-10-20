{-# LANGUAGE OverloadedStrings #-}
module Distance(fromLocationToDistanceDoc) where

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import           Geo.Computations (distance, pt)
import           Geo.Types        (Point)
import           Geocoding        (Location (..))
import           Poi              (Stop (..), StopType (..), Store (..),
                                   StoreType (..))
data Distances = Distances
                    { toColes     :: Integer
                    , toWoolworth :: Integer
                    , toAldi      :: Integer
                    , toTrain     :: Integer
                    , toTram      :: Integer
                    , toBus       :: Integer
                    } deriving (Show)

fromLocationToDistanceDoc :: [Stop] -> [Store] -> Double -> Double -> Mongo.Document
fromLocationToDistanceDoc stops stores latitude longitude =
  toDoc $ fromLocation stops stores latitude longitude

toDoc :: Distances -> Mongo.Document
toDoc distances =
  [ "toColes" =: toColes distances
  , "toWoolworth" =: toWoolworth distances
  , "toAldi"      =: toAldi distances
  , "toTrain"     =: toTrain distances
  , "toTram"      =: toTram distances
  , "toBus"       =: toBus distances
  ]

fromLocation :: [Stop] -> [Store] -> Double -> Double -> Distances
fromLocation stops stores latitude longitude = Distances
    { toColes = dtColes
    , toWoolworth = dtWoolworth
    , toAldi = dtAldi
    , toTrain = dtTrain
    , toTram = dtTram
    , toBus = dtBus
    }
    where
      point = toPoint latitude longitude
      distToStore = calculateDistToStore point stores
      distToStop = calculateDistToStop point stops
      dtColes = distToStore Coles
      dtWoolworth = distToStore Woolworths
      dtAldi = distToStore Aldi
      dtTrain = distToStop Train
      dtTram = distToStop Tram
      dtBus = distToStop Bus

calculateDistToStore :: Point -> [Store] -> StoreType -> Integer
calculateDistToStore propertyLocation stores sType = round closest
    where
        filteredStores = filter (\s -> storeType s == sType) stores
        distances = fmap (\s ->
            distance (toPoint (storeLatitude s) (storeLongitude s)) propertyLocation
            ) filteredStores
        closest = minimum distances

calculateDistToStop :: Point -> [Stop] -> StopType -> Integer
calculateDistToStop propertyLocation stops sType = round closest
    where
        filteredStops = filter (\s -> stopType s == sType) stops
        distances = fmap (\s ->
            distance (toPoint (stopLatitude s) (stopLongitude s)) propertyLocation
            ) filteredStops
        closest = minimum distances

toPoint :: Double -> Double -> Point
toPoint latitude longitude =
    pt latitude longitude Nothing Nothing

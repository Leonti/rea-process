{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Geocoding(geocodeAddress, Result(..), Location(..)) where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON, FromJSON, decode)
--import Data.ByteString.Lazy.UTF8(fromString)
import Data.ByteString.Lazy.Internal(ByteString)


import HttpsClient(fetchUrl)
import Network.HTTP(urlEncode)
import           System.Environment (getEnv)

data Location = LatLng
  { lat :: Double
  , lng :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

data Geometry = Geometry
  { location :: Location
  , location_type :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data GeocodingResult = GeocodingResult
  { formatted_address :: String
  , geometry :: Geometry
  } deriving (Show, Generic, ToJSON, FromJSON)

data GeocodingResults = GeocodingResults
  { results :: [GeocodingResult]
  , status :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data Result = Result
  { latitude :: Double
  , longitude :: Double
  , formattedAddress :: String
  } deriving (Show)

decodeGeocodingResponse :: ByteString -> Maybe GeocodingResults
decodeGeocodingResponse inputResponse = Data.Aeson.decode inputResponse :: Maybe GeocodingResults

--openUrl :: String -> IO String
--openUrl x = getResponseBody =<< simpleHTTP (getRequest x)

geocodingUrl :: String -> String -> String
geocodingUrl address key = baseUrl ++ encodedAddress ++ encodedKey-- ++ encodedBounds
  where
    baseUrl = "https://maps.googleapis.com/maps/api/geocode/json?"
    encodedAddress = "&address=" ++ urlEncode (address ++ ", Australia")
    encodedKey = "&key=" ++ key
--    encodedBounds = "&bounds=" ++ "-37.8555269395,144.95143536|-37.799716676,144.990094688"

extractResult :: GeocodingResults -> Maybe Result
extractResult response = case status response of
  "OK" -> Just Result
    { latitude = lat l
    , longitude = lng l
    , formattedAddress = fa
    }
    where
      l = (location . geometry) (head $ results response)
      fa = formatted_address (head $ results response)
  _ -> Nothing

geocodeAddress :: String -> IO (Maybe Result)
geocodeAddress address = do
  geocodingKey <- getEnv "GEOCODING_KEY"
  geocodingResponse <- fetchUrl $ geocodingUrl address geocodingKey
  let parsedResults = geocodingResponse >>= decodeGeocodingResponse
  let maybeResult = parsedResults >>= extractResult
  _ <- print $ show parsedResults
  return maybeResult

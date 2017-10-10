{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Geocoding(geocodeAddress, Result) where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON, FromJSON, decode)
import Data.ByteString.Lazy.UTF8(fromString)

import Network.HTTP(urlEncode, getResponseBody, simpleHTTP, getRequest)

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
  { loc :: Location
  , formattedAddress :: String
  } deriving (Show)

decodeGeocodingResponse :: String -> Maybe GeocodingResults
decodeGeocodingResponse inputResponse = Data.Aeson.decode (fromString inputResponse) :: Maybe GeocodingResults

openUrl :: String -> IO String
openUrl x = getResponseBody =<< simpleHTTP (getRequest x)

geocodingUrl :: String -> String
geocodingUrl address = baseUrl ++ encodedAddress
  where
    baseUrl = "http://maps.googleapis.com/maps/api/geocode/json?"
    encodedAddress = "&address=" ++ urlEncode (address ++ ", Australia")

extractResult :: GeocodingResults -> Maybe Result
extractResult response = case status response of
  "OK" -> Just Result
    { loc = l
    , formattedAddress = fa
    }
    where
      l = (location . geometry) (head $ results response)
      fa = formatted_address (head $ results response)
  _ -> Nothing

geocodeAddress :: String -> IO (Maybe Result)
geocodeAddress address = do
  geocodingResponse <- openUrl $ geocodingUrl address
  let parsedResults = decodeGeocodingResponse geocodingResponse
  let maybeResult = parsedResults >>= extractResult
  _ <- print $ show parsedResults
  _ <- print $ show maybeResult
  return maybeResult

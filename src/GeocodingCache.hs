{-# LANGUAGE OverloadedStrings #-}

module GeocodingCache where

import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import Data.Char(isAscii)
import Geocoding(geocodeAddress, Result(..))
import DbStore(actionToIO, copyField)

geocodeOrGetFromCache :: String -> IO (Maybe Mongo.Document)
geocodeOrGetFromCache location = do
  existingResult <- geocodedForAddress location
  maybeResult <- extractOrFetch location existingResult
  _ <- print $ show maybeResult
  return maybeResult

extractOrFetch :: String -> [Mongo.Document] -> IO (Maybe Mongo.Document)
extractOrFetch location (x:xs) = return $ Just x
extractOrFetch location [] = do
  maybeResult <- geocodeAddress (filter isAscii location)
  case maybeResult of
    (Just result) -> do
      _ <- upsertGeocodingResult resultDoc
      return $ Just resultDoc
      where
        resultDoc = geocodingResultToDoc location result
    Nothing -> return Nothing

geocodedForAddress :: String -> IO [Mongo.Document]
geocodedForAddress location = actionToIO $ Mongo.rest =<< Mongo.find (Mongo.select
    [ "location" =: location ] "geocodingCache")

upsertGeocodingResult :: Mongo.Document -> IO ()
upsertGeocodingResult result = actionToIO $
  Mongo.upsert (Mongo.select existingSelector "geocodingCache") result
  where
    existingSelector = [copyField result "location"]

geocodingResultToDoc :: String -> Result -> Mongo.Document
geocodingResultToDoc location result =
  [ "location" =: location
  , "latitude" =: latitude result
  , "longitude" =: longitude result
  , "formattedAddress" =: formattedAddress result
  ]

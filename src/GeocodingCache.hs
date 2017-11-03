{-# LANGUAGE OverloadedStrings #-}

module GeocodingCache where

import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import Data.Char(isAscii)
import Geocoding(geocodeAddress, Result(..))
import DbStore(actionToIO, copyField)

geocodeOrGetFromCache :: Mongo.Pipe -> String -> IO (Maybe Mongo.Document)
geocodeOrGetFromCache pipe location = do
  existingResult <- geocodedForAddress pipe location
  maybeResult <- extractOrFetch pipe location existingResult
  _ <- print $ show maybeResult
  return maybeResult

extractOrFetch :: Mongo.Pipe -> String -> [Mongo.Document] -> IO (Maybe Mongo.Document)
extractOrFetch pipe location (x:xs) = return $ Just x
extractOrFetch pipe location [] = do
  maybeResult <- geocodeAddress (filter isAscii location)
  case maybeResult of
    (Just result) -> do
      _ <- upsertGeocodingResult pipe resultDoc
      return $ Just resultDoc
      where
        resultDoc = geocodingResultToDoc location result
    Nothing -> return Nothing

geocodedForAddress :: Mongo.Pipe -> String -> IO [Mongo.Document]
geocodedForAddress pipe location = (actionToIO pipe) $ Mongo.rest =<< Mongo.find (Mongo.select
    [ "location" =: location ] "geocodingCache")

upsertGeocodingResult :: Mongo.Pipe -> Mongo.Document -> IO ()
upsertGeocodingResult pipe result = (actionToIO pipe) $
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

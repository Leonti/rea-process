{-# LANGUAGE OverloadedStrings #-}

module DbStore(
uniqueOnSaleLinks,
onSaleForLink,
copyField,
upsertOnSaleProcessed,
upsertSoldProcessed,
extractField,
extractDoubleField,
extractIntegerField,
findSoldById,
fieldToString,
actionToIO,
allSoldProperties,
authenticatedMongoPipe
) where

import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import           Data.Text                 (pack, unpack)
import Geocoding(Result(..))
import           System.Environment (getEnv)
import Data.Maybe(fromJust)
import Data.Time.Clock.POSIX(getPOSIXTime)

actionToIO :: Mongo.Pipe -> Mongo.Action IO a -> IO a
actionToIO pipe action = do
    mongoDb <- getEnv "MONGO_DB"
    Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) action

authenticatedMongoPipe :: IO Mongo.Pipe
authenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe

allSoldProperties :: Mongo.Pipe -> IO [Mongo.Document]
allSoldProperties pipe = (actionToIO pipe)  $ Mongo.rest =<< Mongo.find (Mongo.select [] "soldProperties")

copyField :: Mongo.Document -> Mongo.Label -> Mongo.Field
copyField doc label = label =: Mongo.valueAt label doc

uniqueOnSaleLinks :: Mongo.Pipe -> IO [Mongo.Value]
uniqueOnSaleLinks pipe = (actionToIO pipe) $ Mongo.distinct "link" (Mongo.select [] "properties")

onSaleForLink :: Mongo.Pipe -> Mongo.Value -> IO [Mongo.Document]
onSaleForLink pipe link = (actionToIO pipe) $ Mongo.rest =<< Mongo.find (Mongo.select
    [ "link" =: link ] "properties")

findSoldById :: Mongo.Pipe -> Mongo.Value -> IO [Mongo.Document]
findSoldById pipe propertyId = (actionToIO pipe) $ Mongo.rest =<< Mongo.find (Mongo.select
    ["link" =: ["$regex" =: propertyId]] "soldProperties")

extractIntegerField :: String -> Mongo.Document -> Integer
extractIntegerField label doc = fieldToInteger $ Mongo.valueAt (pack label) doc

fieldToInteger :: Mongo.Value -> Integer
fieldToInteger number = fromJust (Mongo.cast number :: Maybe Integer)

extractDoubleField :: String -> Mongo.Document -> Double
extractDoubleField label doc = fieldToDouble $ Mongo.valueAt (pack label) doc

fieldToDouble :: Mongo.Value -> Double
fieldToDouble number = fromJust (Mongo.cast number :: Maybe Double)

extractField :: String -> Mongo.Document -> String
extractField label doc = fieldToString $ Mongo.valueAt (pack label) doc

fieldToString :: Mongo.Value -> String
fieldToString (Mongo.String s) = unpack s
fieldToString _            = error "Value is not a string"

upsertProcessed :: Mongo.Pipe -> String -> Mongo.Document -> IO ()
upsertProcessed pipe collection processed = (actionToIO pipe) $
  Mongo.upsert (Mongo.select existingSelector (pack collection)) processed
  where
    existingSelector = [copyField processed "link"]

upsertOnSaleProcessed :: Mongo.Pipe -> Mongo.Document -> IO ()
upsertOnSaleProcessed pipe = upsertProcessed pipe "processedOnSaleProperties"

upsertSoldProcessed :: Mongo.Pipe -> Mongo.Document -> IO ()
upsertSoldProcessed pipe = upsertProcessed pipe "processedSoldProperties"

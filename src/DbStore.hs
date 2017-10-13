{-# LANGUAGE OverloadedStrings #-}

module DbStore(
uniqueOnSaleLinks,
onSaleForLink,
copyField,
upsertOnSaleProcessed,
extractField,
findSoldById,
fieldToString,
actionToIO
) where

import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import           Data.Text                 (pack, unpack)
import Geocoding(Result(..))

import           System.Environment (getEnv)

actionToIO :: Mongo.Action IO a -> IO a
actionToIO action = do
    pipe <- authenticatedMongoPipe
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

copyField :: Mongo.Document -> Mongo.Label -> Mongo.Field
copyField doc label = label =: Mongo.valueAt label doc

uniqueOnSaleLinks :: IO [Mongo.Value]
uniqueOnSaleLinks = actionToIO $ Mongo.distinct "link" (Mongo.select [] "properties")

onSaleForLink :: Mongo.Value -> IO [Mongo.Document]
onSaleForLink link = actionToIO $ Mongo.rest =<< Mongo.find (Mongo.select
    [ "link" =: link ] "properties")

findSoldById :: Mongo.Value -> IO [Mongo.Document]
findSoldById propertyId = actionToIO $ Mongo.rest =<< Mongo.find (Mongo.select
    ["link" =: ["$regex" =: propertyId]] "soldProperties")

extractField :: String -> Mongo.Document -> String
extractField label doc = fieldToString $ Mongo.valueAt (pack label) doc

fieldToString :: Mongo.Value -> String
fieldToString (Mongo.String s) = unpack s
fieldToString _            = error "Value is not a string"

upsertOnSaleProcessed :: Mongo.Document -> IO ()
upsertOnSaleProcessed processed = actionToIO $
  Mongo.upsert (Mongo.select existingSelector "processedOnSaleProperties") processed
  where
    existingSelector = [copyField processed "link"]

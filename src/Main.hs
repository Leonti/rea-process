{-# LANGUAGE OverloadedStrings #-}
import DbStore(
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
  allSoldProperties,
  authenticatedMongoPipe)


import Safe(headMay)
import qualified Database.MongoDB          as Mongo
import           Database.MongoDB          ((=:))
import           Time(toTimestamp)
import Data.List(sortOn)
import Parsing(extractId)
import GeocodingCache(geocodeOrGetFromCache)
import Poi(Stop, Store, lineToStop, lineToStore)
import Distance(fromLocationToDistanceDoc)
import Control.Concurrent.Spawn(pool, parMapIO)

main :: IO ()
main = do
  _ <- putStrLn "Start processing"
  wrap <- pool 100
  pipe <- authenticatedMongoPipe
  stops <- readStops
  stores <- readStores
  links <- uniqueOnSaleLinks pipe
  _ <- parMapIO (wrap . (processLink pipe stops stores)) links
  soldProperties <- allSoldProperties pipe
  _ <- parMapIO (wrap . (processSoldProperty pipe stops stores)) soldProperties
  putStrLn $ "Links count " ++ show (length links)

processSoldProperty :: Mongo.Pipe -> [Stop] -> [Store] -> Mongo.Document -> IO ()
processSoldProperty pipe stops stores soldProperty = do
  maybeGeocoding <- geocodeOrGetFromCache pipe (extractField "location" soldProperty)
  let maybeDistances = fmap (geocodingToDistances stops stores) maybeGeocoding
  let updatedSoldProperty = updateSoldProperty soldProperty maybeGeocoding maybeDistances
  _ <- upsertSoldProcessed pipe updatedSoldProperty
  print ("Processed sold property " ++ extractField "link" soldProperty)

updateSoldProperty :: Mongo.Document -> Maybe Mongo.Document -> Maybe Mongo.Document -> Mongo.Document
updateSoldProperty soldProperty maybeGeocoding maybeDistances =
  Mongo.merge soldProperty (Mongo.merge (Mongo.merge geocodingDoc distancesDoc) timestampDoc)
  where
    timestampDoc = ["timestamp" =: toTimestamp (extractField "soldAt" soldProperty)]
    geocodingDoc = maybe [] (\g -> [ "geo" =: g ]) maybeGeocoding
    distancesDoc = maybe [] (\d -> [ "distances" =: d ]) maybeDistances

processLink :: Mongo.Pipe -> [Stop] -> [Store] -> Mongo.Value -> IO ()
processLink pipe stops stores link = do
  onSale <- onSaleForLink pipe link
  let propertyId = extractId $ fieldToString link
  soldById <- findSoldById pipe (Mongo.val propertyId)
  maybeGeocoding <- geocodeOrGetFromCache pipe (extractAddress onSale)
  let maybeDistances = fmap (geocodingToDistances stops stores) maybeGeocoding
  let onSaleProcessed = toOnSaleProcessedDoc onSale soldById maybeGeocoding maybeDistances
  _ <- upsertOnSaleProcessed pipe onSaleProcessed
  putStrLn $ "processing link" ++ show link

geocodingToDistances :: [Stop] -> [Store] -> Mongo.Document -> Mongo.Document
geocodingToDistances stops stores geocodingDoc =
  fromLocationToDistanceDoc stops stores latitude longitude
  where
    latitude = extractDoubleField "latitude" geocodingDoc
    longitude = extractDoubleField "longitude" geocodingDoc

toDatePrice :: Mongo.Document -> Mongo.Document
toDatePrice doc =
  [ copyField doc "price"
  , "timestamp" =: toTimestamp (extractField "extractedDate" doc)
  ]

--toLocalTime :: String -> Maybe LocalTime
--toLocalTime dateAsString = fst <$> strptime "%Y-%m-%d %H:%M" (dateAsString ++ " 22:30")

extractAddress :: [Mongo.Document] -> String
extractAddress onSaleList = extractField "location" $ last sortedOnSale
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList

toOnSaleProcessedDoc :: [Mongo.Document] -> [Mongo.Document] -> Maybe Mongo.Document -> Maybe Mongo.Document -> Mongo.Document
toOnSaleProcessedDoc onSaleList soldResult maybeGeocoding maybeDistances =
  Mongo.merge doc (Mongo.merge geocodingDoc distancesDoc)
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList
    copy = copyField $ last sortedOnSale
    datesPrices = fmap toDatePrice sortedOnSale
    doc =
      [ copy "link"
      , copy "extractedDate"
      , "extractedAt" =: lastExtractedAt
      , copy "bedrooms"
      , copy "bathrooms"
      , copy "cars"
      , copy "location"
      , "datesPrices" =: datesPrices
      , "isSold" =: isSold
      , "salePrice" =: maybeSoldPrice
      , "soldAt" =: maybeSoldTimestamp
      ]
    geocodingDoc = maybe [] (\g -> [ "geo" =: g ]) maybeGeocoding
    distancesDoc = maybe [] (\d -> [ "distances" =: d ]) maybeDistances
    isSold = not (null soldResult)
    maybeSold = headMay soldResult
    maybeSoldTimestamp = fmap (toTimestamp . extractField "soldAt") maybeSold
    maybeSoldPrice = fmap (extractIntegerField "price") maybeSold
    lastExtractedAt = (toTimestamp . extractField "extractedDate") (last sortedOnSale)

readStops :: IO [Stop]
readStops = do
    contents <- readFile "stops.csv"
    return $ map lineToStop $ lines contents

readStores :: IO [Store]
readStores = do
    contents <- readFile "stores.csv"
    return $ map lineToStore $ lines contents

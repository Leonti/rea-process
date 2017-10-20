{-# LANGUAGE OverloadedStrings #-}
import DbStore(
  uniqueOnSaleLinks,
  onSaleForLink,
  copyField,
  upsertOnSaleProcessed,
  upsertSoldProcessed,
  extractField,
  extractDoubleField,
  findSoldById,
  fieldToString,
  allSoldProperties)

import qualified Database.MongoDB          as Mongo
import           Database.MongoDB          ((=:))
import           Time(toTimestamp)
import Data.List(sortOn)
import Parsing(extractId)
import GeocodingCache(geocodeOrGetFromCache)
import Poi(Stop, Store, lineToStop, lineToStore)
import Distance(fromLocationToDistanceDoc)

main :: IO ()
main = do
  stops <- readStops
  stores <- readStores
  links <- uniqueOnSaleLinks
--  _ <- mapM (processLink stops stores) links
  soldProperties <- allSoldProperties
  _ <- mapM (processSoldProperty stops stores) soldProperties
  putStrLn $ "Links count " ++ show (length links)

processSoldProperty :: [Stop] -> [Store] -> Mongo.Document -> IO ()
processSoldProperty stops stores soldProperty = do
  maybeGeocoding <- geocodeOrGetFromCache (extractField "location" soldProperty)
  let maybeDistances = fmap (geocodingToDistances stops stores) maybeGeocoding
  let updatedSoldProperty = updateSoldProperty soldProperty maybeGeocoding maybeDistances
  _ <- upsertSoldProcessed updatedSoldProperty
  print ("Processed sold property " ++ extractField "link" soldProperty)

updateSoldProperty :: Mongo.Document -> Maybe Mongo.Document -> Maybe Mongo.Document -> Mongo.Document
updateSoldProperty soldProperty maybeGeocoding maybeDistances =
  Mongo.merge soldProperty (Mongo.merge geocodingDoc distancesDoc)
  where
    timeStapmpDoc = ["timestamp" =: toTimestamp (extractField "soldAt" soldProperty)]
    geocodingDoc = maybe [] (\g -> [ "geo" =: g ]) maybeGeocoding
    distancesDoc = maybe [] (\d -> [ "distances" =: d ]) maybeDistances

processLink :: [Stop] -> [Store] -> Mongo.Value -> IO ()
processLink stops stores link = do
  onSale <- onSaleForLink link
  let propertyId = extractId $ fieldToString link
  soldById <- findSoldById (Mongo.val propertyId)
  maybeGeocoding <- geocodeOrGetFromCache (extractAddress onSale)
  let maybeDistances = fmap (geocodingToDistances stops stores) maybeGeocoding
  let onSaleProcessed = toOnSaleProcessedDoc onSale (not (null soldById)) maybeGeocoding maybeDistances
  _ <- upsertOnSaleProcessed onSaleProcessed
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
  , copyField doc "extractedDate"
  , "timestamp" =: toTimestamp (extractField "extractedDate" doc)
  ]

--toLocalTime :: String -> Maybe LocalTime
--toLocalTime dateAsString = fst <$> strptime "%Y-%m-%d %H:%M" (dateAsString ++ " 22:30")

extractAddress :: [Mongo.Document] -> String
extractAddress onSaleList = extractField "location" $ last sortedOnSale
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList

toOnSaleProcessedDoc :: [Mongo.Document] -> Bool -> Maybe Mongo.Document -> Maybe Mongo.Document -> Mongo.Document
toOnSaleProcessedDoc onSaleList isSold maybeGeocoding maybeDistances =
  Mongo.merge doc (Mongo.merge geocodingDoc distancesDoc)
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList
    copy = copyField $ last sortedOnSale
    datesPrices = fmap toDatePrice sortedOnSale
    doc =
      [ copy "link"
      , copy "extractedDate"
      , copy "bedrooms"
      , copy "bathrooms"
      , copy "cars"
      , copy "location"
      , "datesPrices" =: datesPrices
      , "isSold" =: isSold
      ]
    geocodingDoc = maybe [] (\g -> [ "geo" =: g ]) maybeGeocoding
    distancesDoc = maybe [] (\d -> [ "distances" =: d ]) maybeDistances

readStops :: IO [Stop]
readStops = do
    contents <- readFile "stops.csv"
    return $ map lineToStop $ lines contents

readStores :: IO [Store]
readStores = do
    contents <- readFile "stores.csv"
    return $ map lineToStore $ lines contents

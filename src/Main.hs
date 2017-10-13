{-# LANGUAGE OverloadedStrings #-}
import DbStore(
  uniqueOnSaleLinks,
  onSaleForLink,
  copyField,
  upsertOnSaleProcessed,
  extractField,
  findSoldById,
  fieldToString)

import qualified Database.MongoDB          as Mongo
import           Database.MongoDB          ((=:))
import           Time(toTimestamp)
import Data.List(sortOn)
import Parsing(extractId)
import GeocodingCache(geocodeOrGetFromCache)

main :: IO ()
main = do
  links <- uniqueOnSaleLinks
  processed <- mapM processLink links
  putStrLn $ "Links count " ++ show (length links)

processLink :: Mongo.Value -> IO ()
processLink link = do
  onSale <- onSaleForLink link
  let propertyId = extractId $ fieldToString link
  soldById <- findSoldById (Mongo.val propertyId)
  let onSaleProcessed = toOnSaleProcessedDoc onSale (not (null soldById))
  _ <- upsertOnSaleProcessed $ toOnSaleProcessedDoc onSale (not (null soldById))
  putStrLn $ "processing link" ++ show link

toDatePrice :: Mongo.Document -> Mongo.Document
toDatePrice doc =
  [ copyField doc "price"
  , copyField doc "extractedDate"
  , "timestamp" =: toTimestamp (extractField "extractedDate" doc)
  ]

--toLocalTime :: String -> Maybe LocalTime
--toLocalTime dateAsString = fst <$> strptime "%Y-%m-%d %H:%M" (dateAsString ++ " 22:30")

toOnSaleProcessedDoc :: [Mongo.Document] -> Bool -> Mongo.Document
toOnSaleProcessedDoc onSaleList isSold =
  [ copy "link"
  , copy "extractedDate"
  , copy "bedrooms"
  , copy "bathrooms"
  , copy "cars"
  , copy "location"
  , "datesPrices" =: datesPrices
  , "isSold" =: isSold
  ]
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList
    copy = copyField $ last sortedOnSale
    datesPrices = fmap toDatePrice sortedOnSale

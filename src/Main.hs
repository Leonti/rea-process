{-# LANGUAGE OverloadedStrings #-}
import DbStore(
  uniqueOnSaleLinks,
  onSaleForLink,
  copyField,
  upsertOnSaleProcessed,
  extractField)

import qualified Database.MongoDB          as Mongo
import           Database.MongoDB          ((=:))
import           Time(toTimestamp)
import Data.List(sortOn)

main :: IO ()
main = do
  links <- uniqueOnSaleLinks
  processed <- mapM processLink links
  putStrLn $ "Links count " ++ show links

processLink :: Mongo.Value -> IO ()
processLink link = do
  onSale <- onSaleForLink link
  let onSaleProcessed = toOnSaleProcessedDoc onSale
  _ <- upsertOnSaleProcessed $ toOnSaleProcessedDoc onSale
  putStrLn $ "processing link" ++ show link

toDatePrice :: Mongo.Document -> Mongo.Document
toDatePrice doc =
  [ copyField doc "price"
  , copyField doc "extractedDate"
  , "timestamp" =: toTimestamp (extractField "extractedDate" doc)
  ]

--toLocalTime :: String -> Maybe LocalTime
--toLocalTime dateAsString = fst <$> strptime "%Y-%m-%d %H:%M" (dateAsString ++ " 22:30")

toOnSaleProcessedDoc :: [Mongo.Document] -> Mongo.Document
toOnSaleProcessedDoc onSaleList =
  [ copy "link"
  , copy "extractedDate"
  , copy "bedrooms"
  , copy "bathrooms"
  , copy "cars"
  , copy "location"
  , "datesPrices" =: datesPrices
  ]
  where
    sortedOnSale = sortOn (toTimestamp . extractField "extractedDate") onSaleList
    copy = copyField $ last sortedOnSale
    datesPrices = fmap toDatePrice sortedOnSale

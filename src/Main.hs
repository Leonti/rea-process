{-# LANGUAGE OverloadedStrings #-}
import DbStore(uniqueOnSaleLinks, onSaleForLink, copyField, upsertOnSaleProcessed)

import qualified Database.MongoDB          as Mongo
import           Database.MongoDB          ((=:))

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
  ]

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
    lastOnSale = last onSaleList
    copy = copyField lastOnSale
    datesPrices = fmap toDatePrice onSaleList

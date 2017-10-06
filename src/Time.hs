module Time(toTimestamp) where

import Data.ByteString.Char8(pack)
import Data.UnixTime(parseUnixTimeGMT, UnixTime, utSeconds)

toTimestamp :: String -> Integer
toTimestamp dateAsString = (read . show) $ utSeconds parsed
  where
    parsed = parseUnixTimeGMT (pack "%Y-%m-%d %H:%M") (pack $ dateAsString ++ " 22:30")

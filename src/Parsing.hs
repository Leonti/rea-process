module Parsing where

import Text.Regex.TDFA(AllTextMatches, getAllTextMatches, (=~))

extractId :: String -> String
extractId link = head matches
  where
    matches :: [String]
    matches = getAllTextMatches (link =~ "[0-9]+" :: AllTextMatches [] String)

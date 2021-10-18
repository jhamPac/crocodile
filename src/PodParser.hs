module PodParser where

import           Data.Char
import           Data.List
import           PodTypes
import           Text.XML.HaXml
import           Text.XML.HaXml.Html.Generate (showattr)
import           Text.XML.HaXml.Parse

data PodItem = PodItem {itemtitle :: String, enclosureURL :: String} deriving (Eq, Show, Read)

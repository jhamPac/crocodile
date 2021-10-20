module PodDownload where

import           Data.Maybe
import           Database.HDBC
import           Network.HTTP
import           Network.URI
import           PodDB
import           PodParser
import           PodTypes
import           System.IO

downloadURL :: String -> IO (Either String String)
downloadURL = undefined

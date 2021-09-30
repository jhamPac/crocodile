module PodTypes where

data Podcast =
    Podcast {castID :: Integer, castURL :: String} deriving (Eq, Show, Read)

data Episode =
    Episode {
        epID   :: Integer,
        epCast :: Podcast,
        epURL  :: String,
        epDone :: Bool
    } deriving (Eq, Show, Read)

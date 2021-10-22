module Main where

import           Database.HDBC
import           Network.Socket     (withSocketsDo)
import           PodDB
import           PodDownload
import           PodTypes
import           System.Environment

main :: IO ()
main = withSocketsDo $ handleSqlError $ do
    args <- getArgs
    conn <- connect "testpoddb.db"
    case args of
        ["add", url] -> add conn url
        ["update"] ->    update conn
        ["download"] ->  download conn
        ["fetch"] -> do update conn
                        download conn
        _ -> syntaxError
    disconnect conn

add :: IConnection conn => conn -> [Char] -> IO ()
add conn url = do
    addPodcast conn pc
    commit conn
    where
        pc = Podcast { castID = 0, castURL = url}

update conn = do
    pclist <- getPodcasts conn
    mapM_ procPodcast pclist
    where
        procPodcast pc = do
            putStrLn $ "Updating from " ++ castURL pc
            updatePodcastFromFeed conn pc

download conn = do
    pclist <- getPodcasts conn
    mapM_ procPodcast pclist
    where
        procPodcast pc = do
            putStrLn $ "Considering " ++ castURL pc
            episodelist <- getPodcastEpisodes conn pc
            let dleps = filter (\ep -> epDone ep == False) episodelist
            mapM_ procEpisode dleps

        procEpisode ep = do
            putStrLn $ "Downloading " ++ epURL ep
            getEpisode conn ep

syntaxError = putStrLn "usage is wrong!"

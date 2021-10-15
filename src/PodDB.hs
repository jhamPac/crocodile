module PodDB where

import           Control.Monad         (unless, void, when)
import           Data.List             (sort)
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           PodTypes              (Episode (epCast, epDone, epID, epURL),
                                        Podcast (..))

connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    unless (elem "podcasts" tables) $ do
        run dbh "CREATE TABLE podcasts (\
                    \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \casturl TEXT NOT NULL UNIQUE)" []
        return ()

    unless (elem "episodes" tables) $ do
        run dbh "CREATE TABLE episodes (\
                    \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \epcastid INTEGER NOT NULL,\
                    \epurl TEXT NOT NULL,\
                    \epdone INTEGER NOT NULL,\
                    \UNIQUE(epcastid, epurl),\
                    \UNIQUE(epcastid, epid))" []
        return ()

    commit dbh

addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast =
    handleSql errorHandler $ do
        run dbh "INSERT INTO podcasts (casturl) VALUES (?)" [toSql (castURL podcast)]
        r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE casturl = ?" [toSql (castURL podcast)]
        case r of
            [[x]] -> return $ podcast {castID = fromSql x}
            y     -> fail $ "addPodcast: unexpected result: " ++ show y
    where errorHandler e =
            do fail $ "Error adding podcast; does this URL already exist?\n" ++ show e

addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epcastid, epurl, epdone) \
                \Values (?, ?, ?)"
                [toSql (castID . epCast $ ep), toSql (epURL ep), toSql (epDone ep)]
    >> return ()

updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
    let f = run dbh "UPDATE podcasts SET casturl = ? WHERE castid = ?" [toSql (castURL podcast), toSql (castID podcast)]
    in Control.Monad.void f

updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode =
    run dbh "UPDATE episodes SET epcastid = ?, epurl = ?, epdone = ? Where epid = ?"
            [toSql (castID . epCast $ episode),
             toSql (epURL episode),
             toSql (epDone episode),
             toSql (epID episode)]
    >> return ()

removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast conn p = do
    run conn "DELETE FROM episodes WHERE epcastid = ?" [toSql (castID p)]
    run conn "DELETE FROM podcasts WHERE castid = ?" [toSql(castID p)]
    return ()

getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts conn = do
    results <- quickQuery' conn "SELECT castid, casturl FROM podcasts ORDER BY castid" []
    return (map convPodcastRow results)

convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svID, svURL] =
    Podcast {castID = fromSql svID, castURL = fromSql svURL}

convPodcastRow s = error $ "Can't convert podcast row " ++ show s

module PodDB where

import           Control.Monad         (unless, void, when)
import           Data.List             (sort)
import           Database.HDBC         (IConnection (commit, getTables, run),
                                        SqlValue, fromSql, handleSql,
                                        quickQuery', toSql)
import           Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import           PodTypes              (Episode (..), Podcast (..))

connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    pure dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    unless (elem "podcasts" tables) $ do
        run dbh "CREATE TABLE podcasts (\
                    \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \casturl TEXT NOT NULL UNIQUE)" []
        pure ()

    unless (elem "episodes" tables) $ do
        run dbh "CREATE TABLE episodes (\
                    \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \epcastid INTEGER NOT NULL,\
                    \epurl TEXT NOT NULL,\
                    \epdone INTEGER NOT NULL,\
                    \UNIQUE(epcastid, epurl),\
                    \UNIQUE(epcastid, epid))" []
        pure ()

    commit dbh

addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast =
    handleSql errorHandler $ do
        run dbh "INSERT INTO podcasts (casturl) VALUES (?)" [toSql (castURL podcast)]
        r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE casturl = ?" [toSql (castURL podcast)]
        case r of
            [[x]] -> pure $ podcast {castID = fromSql x}
            y     -> fail $ "addPodcast: unexpected result: " ++ show y
    where errorHandler e =
            do fail $ "Error adding podcast; does this URL already exist?\n" ++ show e

addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epcastid, epurl, epdone) \
                \Values (?, ?, ?)"
                [toSql (castID . epCast $ ep), toSql (epURL ep), toSql (epDone ep)]
    >> pure ()

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
    >> pure ()

removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast conn p = do
    run conn "DELETE FROM episodes WHERE epcastid = ?" [toSql (castID p)]
    run conn "DELETE FROM podcasts WHERE castid = ?" [toSql(castID p)]
    pure ()

getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts conn = do
    results <- quickQuery' conn "SELECT castid, casturl FROM podcasts ORDER BY castid" []
    pure (map convPodcastRow results)

getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast conn id = do
    res <- quickQuery' conn "SELECT castid, casturl FROM podcasts WHERE castid = ?" [toSql id]
    case res of
        [x] -> pure (Just (convPodcastRow x))
        []  -> pure Nothing
        x   -> fail "Some DB error"

convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svID, svURL] =
    Podcast { castID = fromSql svID, castURL = fromSql svURL }

convPodcastRow s = error $ "Can't convert podcast row " ++ show s

getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes conn pc = do
    r <- quickQuery' conn "SELECT epid, epurl, epdone FROM episodes WHERE epcastid = ?" [toSql (castID pc)]
    pure (map convEpisodeRow r)
    where
        convEpisodeRow [svID, svURL, svDone] =
            Episode { epID = fromSql svID
                    , epURL = fromSql svURL
                    , epDone = fromSql svDone
                    , epCast = pc }

        convEpisodeRow s = error $ "Can't convert episodes row " ++ show s

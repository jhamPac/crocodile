module Query where

import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3)

query :: Int -> IO ()
query i =
    do
        conn <- connectSqlite3 "h.db"
        -- run conn "CREATE TABLE food (id INTEGER NOT NULL, desc VARCHAR(80))" []
        -- run conn "INSERT INTO food VALUES (?, ?)" [toSql "0", toSql "apple"]
        -- stmt <- prepare conn "INSERT INTO food VALUES (?, ?)"
        -- execute stmt [toSql "1", toSql "rice"]
        -- execute stmt [toSql "2", toSql "cheese"]
        -- execute stmt [toSql "3", toSql "bread"]
        -- commit conn
        -- disconnect conn

        -- run query and store results in r
        r <- quickQuery' conn
                "SELECT id, desc FROM food WHERE id <= ? ORDER BY id, desc" [toSql i]

        -- convert each row into a string
        let rows = map convRow r

        -- mapM_ execute monadic action on each element
        mapM_ putStrLn rows

        disconnect conn

        where
            convRow :: [SqlValue] -> String
            convRow [sqlId, sqlDesc] =
                show intid ++ ": " ++ desc
                where
                    intid = (fromSql sqlId) :: Integer
                    desc = case fromSql sqlDesc of
                        Just x  -> x
                        Nothing -> "NULL"

            convRow x = fail $ "Unexpected result: " ++ show x

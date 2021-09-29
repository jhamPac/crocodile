module Query where

import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3)

query :: IO ()
query =
    do
        conn <- connectSqlite3 "h.db"
        run conn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []
        run conn "INSERT INTO test VALUES (?, ?)" [toSql "0", toSql "zero"]
        commit conn
        disconnect conn

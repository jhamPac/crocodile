module PodDownload where

import           Data.Maybe    (fromJust)
import           Database.HDBC (IConnection (commit))
import           Network.HTTP  (HeaderName (HdrLocation),
                                Request (Request, rqBody, rqHeaders, rqMethod, rqURI),
                                RequestMethod (GET),
                                Response (rspBody, rspCode), findHeader,
                                simpleHTTP)
import           Network.URI   (parseURI)
import           PodDB         (addEpisode, updateEpisode)
import           PodParser     (Feed (items), item2ep, parse)
import           PodTypes      (Episode (epCast, epDone, epID, epURL),
                                Podcast (castID, castURL))
import           System.IO     (IOMode (WriteMode), hClose, hPutStr,
                                openBinaryFile)

downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x -> pure $ Left ("Error connecting: " ++ show x)
        Right r ->
            case rspCode r of
                (2, _, _) -> pure $ Right (rspBody r)
                (3, _, _) ->
                    case findHeader HdrLocation r of
                        Nothing  -> pure $ Left (show r)
                        Just url -> downloadURL url
                _ -> pure $ Left (show r)
    where
        request = Request { rqURI = uri
                          , rqMethod = GET
                          , rqHeaders = []
                          , rqBody = "" }
        uri = fromJust $ parseURI url

updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed conn pc = do
    resp <- downloadURL (castURL pc)
    case resp of
        Left x    -> putStrLn x
        Right doc -> updateDB doc

    where
        updateDB doc = do
            mapM_ (addEpisode conn) episodes
            commit conn

            where
                feed = parse doc (castURL pc)
                episodes = map (item2ep pc) (items feed)

getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode conn ep = do
    resp <- downloadURL (epURL ep)
    case resp of
        Left x -> do
                    putStrLn x
                    pure Nothing
        Right doc -> do
            file <- openBinaryFile filename WriteMode
            hPutStr file doc
            hClose file
            updateEpisode conn (ep {epDone = True})
            commit conn
            pure (Just filename)

    where
        filename = "pod." ++ (show . castID . epCast $ ep) ++ "." ++ show (epID ep) ++ ".mp3"

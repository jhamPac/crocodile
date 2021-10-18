module PodParser where

import           Data.Char
import           Data.List
import           PodTypes
import           Text.XML.HaXml
import           Text.XML.HaXml.Html.Generate (showattr)
import           Text.XML.HaXml.Parse
import           Text.XML.HaXml.Posn

data PodItem = PodItem {
                        itemTitle      :: String
                        , enclosureURL :: String
                        } deriving (Eq, Show, Read)

data Feed = Feed {
                channelTitle :: String
                , items      :: [PodItem]
                } deriving (Eq, Show, Read)

item2ep :: Podcast -> PodItem -> Episode
item2ep pc item =
    Episode {epID = 0,
             epCast = pc,
             epURL = enclosureURL item,
             epDone = False}

parse :: String -> String -> Feed
parse content name =
    Feed {channelTitle = getTitle doc, items = getEnclosures doc}
    where
        parseResult = xmlParse name $ stripUnicodeBOM content
        doc = getContent parseResult

        getContent :: Document Posn -> Content Posn
        getContent (Document _ _ e _) = CElem e noPos

        {- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
        stripUnicodeBOM :: String -> String
        stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
        stripUnicodeBOM x                        = x

channel :: CFilter Posn
channel = tag "rss" /> tag "channel"

getTitle :: Content Posn -> String
getTitle doc =
    contentToStringDefault "Untitled Podcast" (channel /> tag "title" /> txt $ doc)

getEnclosures :: Content Posn -> [PodItem]
getEnclosures doc =
    concatMap procPodItem $ getPodItems doc
    where
        procPodItem :: Content Posn -> [PodItem]
        procPodItem item = concatMap (procEnclosure title) enclosure
            where
                title = contentToStringDefault "Untitled Episode" (keep /> tag "title" /> txt $ item)
                enclosure = (keep /> tag "enclosure") item

        getPodItems :: CFilter Posn
        getPodItems = channel /> tag "item"

        procEnclosure :: String -> Content Posn -> [PodItem]
        procEnclosure title enclosure =
            map makePodItem (showattr "url" enclosure)
            where
                makePodItem :: Content Posn -> PodItem
                makePodItem x = PodItem {itemTitle = title, enclosureURL = contentToString [x]}

contentToStringDefault :: String -> [Content Posn] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x    = contentToString x

contentToString :: [Content Posn] -> String
contentToString =
    concatMap procContent
    where
        procContent x =
            verbatim $ keep /> txt $ CElem (unesc . fakeElem $ x) noPos

        fakeElem :: Content Posn -> Element Posn
        fakeElem x = Elem (N "fake") [] [x]

        unesc :: Element Posn -> Element Posn
        unesc = xmlUnEscape stdXmlEscaper

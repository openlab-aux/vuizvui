{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language DeriveGeneric #-}
module Main where

import Data.Text (Text)
import Text.RSS.Syntax
import Text.Feed.Types (Feed(RSSFeed))
import Text.Feed.Export (textFeed)

import qualified Data.Aeson as Json
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BS

-- | Info from the config file
data Config = Config
  { channelName :: Text
  , channelURL :: Text
  }
  deriving (Show, Generic)
instance FromJSON Config where

data Channel = Channel
  { channelInfo :: ChannelInfo
  , channelItems :: [ItemInfo]
  }
  deriving (Show, Generic)
instance FromJSON Channel where

-- | Info fetched from the channel
data ChannelInfo = ChannelInfo

  { channelDescription :: Text
  , channelLastUpdate :: DateString -- TODO
  , channelImage :: Maybe () --RSSImage
  }
  deriving (Show, Generic)
instance FromJSON ChannelInfo where

-- | Info of each channel item
data ItemInfo = ItemInfo
  { itemTitle :: Text
  , itemDescription :: Text
  , itemYoutubeLink :: Text
  , itemCategory :: Text
  , itemTags :: [Text]
  , itemURL :: Text
  , itemSizeBytes :: Integer
  , itemHash :: Text
  }
  deriving (Show, Generic)
instance FromJSON ItemInfo where

-- main = print $ textFeed $ RSSFeed $ toRSS
--   (ChannelInfo
--     { channelDescription = "description"
--     , channelLastUpdate = "some date"
--     , channelImage = nullImage "imageURL" "imageTitle" "imageLink"
--     })
--   []
main :: IO ()
main = do
  input <- BS.getContents
  let (Right rss) = Json.eitherDecode input
  let exConfig = (Config
        { channelName = "channel name"
        , channelURL = "channel url"
        })
  print $ textFeed $ RSSFeed $ toRSS exConfig rss

toRSS :: Config -> Channel -> RSS
toRSS Config{..} Channel{channelInfo, channelItems}  =
  let ChannelInfo{..} = channelInfo in
  (nullRSS channelName channelURL)
  { rssChannel = (nullChannel channelName channelURL)
      { rssDescription = channelDescription
      , rssLastUpdate = Just channelLastUpdate
      , rssImage = Nothing --channelImage TODO
      , rssGenerator = Just "youtube2audiopodcast"
      , rssItems = map rssItem channelItems
      }
  }

  where
    rssItem ItemInfo{..} = (nullItem itemTitle)
      { rssItemLink = Just itemYoutubeLink
      , rssItemDescription = Just itemDescription
      -- rssItemAuthor =
      , rssItemCategories =
        map (\name -> RSSCategory
              { rssCategoryDomain = Nothing
              , rssCategoryAttrs = []
              , rssCategoryValue = name
              }) $ itemCategory : itemTags
      , rssItemEnclosure = Just $ RSSEnclosure
          { rssEnclosureURL = itemURL
          , rssEnclosureLength = Just itemSizeBytes
          , rssEnclosureType = "audio/opus"
          , rssEnclosureAttrs = []
          }
      , rssItemGuid = Just $ RSSGuid
          { rssGuidPermanentURL = Just False
          , rssGuidAttrs = []
          , rssGuidValue = itemHash
          }
      -- TODO: date conversion
      -- https://tools.ietf.org/html/rfc822#section-5.1
      -- , rssItemPubDate 
      }

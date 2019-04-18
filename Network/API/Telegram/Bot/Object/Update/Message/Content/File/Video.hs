module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video (Video (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, object, withObject, (.:), (.:?), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size)

data Video = Video Text Int Int Int (Maybe Size) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Video where
	parseJSON = withObject "Video" $ \v -> Video <$> v .: "file_id"
		<*> v .: "width" <*> v .: "height" <*> v .: "duration"
		<*> v .:?  "thumb" <*> v .:? "mime_type" <*> v .:? "file_size"

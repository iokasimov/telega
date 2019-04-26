module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video (Video (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI (URI)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Duration (Duration)

data Video = Video URI Duration Int Int (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Video where
	parseJSON = withObject "Video" $ \v -> Video
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .: "width" <*> v .: "height"
		<*> v .:? "mime_type" <*> v .:? "file_size"

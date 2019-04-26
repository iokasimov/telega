module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio (Audio (..)) where

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

data Audio = Audio URI Duration (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Audio where
	parseJSON = withObject "Audio" $ \v -> Audio
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .:? "performer" <*> v .:? "title"
		<*> v .:? "mime_type" <*> v .:? "file_size"

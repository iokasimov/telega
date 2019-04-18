module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio (Audio (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, object, withObject, (.:), (.:?), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size)

data Audio = Audio Int (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Size)
	deriving Show

instance FromJSON Audio where
	parseJSON = withObject "Audio" $ \v -> Audio
		<$> v .: "duration" <*> v .:? "performer" <*> v .:? "title"
		<*> v .:? "mime_type" <*> v .:? "file_size" <*> v .:? "thumb"

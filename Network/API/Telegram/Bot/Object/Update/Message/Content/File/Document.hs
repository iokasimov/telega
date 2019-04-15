module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document (Document (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size)

data Document = Document Text (Maybe Size) (Maybe Text) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Document where
	parseJSON = withObject "Document" $ \v -> Document <$> v .: "file_id"
		<*> v .:? "thumb" <*> v .:? "file_name" <*> v .:? "mime_type" <*> v .:? "file_size"

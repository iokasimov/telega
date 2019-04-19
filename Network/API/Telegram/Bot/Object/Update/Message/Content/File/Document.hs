module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document (Document (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Document = Document (Maybe Text) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Document where
	parseJSON = withObject "Document" $ \v -> Document
		<$> v .:? "file_name" <*> v .:? "mime_type" <*> v .:? "file_size"

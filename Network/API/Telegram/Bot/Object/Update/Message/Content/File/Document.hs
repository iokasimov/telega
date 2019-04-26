module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document (Document (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI (URI)
import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

data Document = Document URI (Maybe Text) (Maybe Text) (Maybe Int)
	deriving Show

instance Identifiable Document where
	type Identificator Document = URI
	ident (Document uri _ _ _) = uri

instance FromJSON Document where
	parseJSON = withObject "Document" $ \v -> Document
		<$> v .: "file_id" <*>  v .:? "file_name"
		<*> v .:? "mime_type" <*> v .:? "file_size"

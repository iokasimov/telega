module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio (Audio (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.URI (URI)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Duration (Duration)
import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

data Audio = Audio URI Duration (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) deriving Show

instance Identifiable Audio where
	type Identificator Audio = URI
	ident (Audio uri _ _ _ _ _) = uri

instance FromJSON Audio where
	parseJSON = withObject "Audio" $ \v -> Audio
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .:? "performer" <*> v .:? "title"
		<*> v .:? "mime_type" <*> v .:? "file_size"

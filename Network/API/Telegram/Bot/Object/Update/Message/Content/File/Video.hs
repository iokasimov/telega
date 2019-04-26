module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video (Video (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Duration (Duration)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Filesize (Filesize)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.MIME (MIME)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Height (Height)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Width (Width)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.URI (URI)
import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

data Video = Video URI Duration Height Width (Maybe MIME) (Maybe Filesize) deriving Show

instance Identifiable Video where
	type Identificator Video = URI
	ident (Video uri _ _ _ _ _) = uri

instance FromJSON Video where
	parseJSON = withObject "Video" $ \v -> Video
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .: "width" <*> v .: "height"
		<*> v .:? "mime_type" <*> v .:? "file_size"

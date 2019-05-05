module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio (Audio (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Duration, Filesize, MIME, Title, URI)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Audio = Audio URI Duration (Maybe Text) (Maybe Title) (Maybe MIME) (Maybe Filesize)

instance Accessible Duration Audio where
	access f (Audio uri duration performer title mime fs) =
		(\duration' -> Audio uri duration' performer title mime fs) <$> f duration

instance Identifiable Audio where
	type Identificator Audio = URI
	ident (Audio uri _ _ _ _ _) = uri

instance FromJSON Audio where
	parseJSON = withObject "Audio" $ \v -> Audio
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .:? "performer" <*> v .:? "title"
		<*> v .:? "mime_type" <*> v .:? "file_size"

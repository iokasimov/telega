module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video (Video (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Field (Duration, Filesize, MIME, Height, Width, URI)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Video = Video URI Duration Height Width (Maybe MIME) (Maybe Filesize) deriving Show

instance Accessible Duration Video where
	access f (Video uri duration height width mime fs) =
		(\duration' -> Video uri duration' height width mime fs) <$> f duration

instance Accessible Height Video where
	access f (Video uri duration height width mime fs) =
		(\height' -> Video uri duration height' width mime fs) <$> f height

instance Accessible Width Video where
	access f (Video uri duration height width mime fs) =
		(\width' -> Video uri duration height width' mime fs) <$> f width

instance Identifiable Video where
	type Identificator Video = URI
	ident (Video uri _ _ _ _ _) = uri

instance FromJSON Video where
	parseJSON = withObject "Video" $ \v -> Video
		<$> v .: "file_id" <*> v .: "duration"
		<*> v .: "width" <*> v .: "height"
		<*> v .:? "mime_type" <*> v .:? "file_size"

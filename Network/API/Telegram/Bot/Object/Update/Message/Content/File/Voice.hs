module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice (Voice (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))

import Network.API.Telegram.Bot.Field (Duration, Filesize, MIME, URI)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Voice = Voice URI Duration (Maybe MIME) (Maybe Filesize)

instance Accessible Duration Voice where
	access f (Voice uri duration mime fs) =
		(\duration' -> Voice uri duration' mime fs) <$> f duration

instance Identifiable Voice where
	type Identificator Voice = URI
	ident (Voice uri _ _ _) = uri

instance FromJSON Voice where
	parseJSON = withObject "Voice" $ \v -> Voice <$> v .: "file_id"
		<*> v .: "duration" <*> v .:? "mime_type" <*> v .:? "file_size"

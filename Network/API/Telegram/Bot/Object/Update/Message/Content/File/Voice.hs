module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice (Voice (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Duration (Duration)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.MIME (MIME)
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.URI (URI)
import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

data Voice = Voice URI Duration (Maybe MIME) (Maybe Int) deriving Show

instance Identifiable Voice where
	type Identificator Voice = URI
	ident (Voice uri _ _ _) = uri

instance FromJSON Voice where
	parseJSON = withObject "Voice" $ \v -> Voice <$> v .: "file_id"
		<*> v .: "duration" <*> v .:? "mime_type" <*> v .:? "file_size"

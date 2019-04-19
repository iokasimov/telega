module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Caption as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

data File
	= Audiofile URI Audio
	| Videofile URI Video
	| General URI Document
	| Voicerecord URI Voice
	| Photography [Size]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v ->
		photo v <|> document v <|> audio v <|> video v <|> voice v where

		photo :: Object -> Parser File
		photo v = Photography <$> v .: "photo"

		audio :: Object -> Parser File
		audio v = Audiofile <$> (v .: "audio" >>= parseJSON) <*> v .: "audio"

		document :: Object -> Parser File
		document v = General <$> (v .: "document" >>= parseJSON) <*> v .: "document"

		video :: Object -> Parser File
		video v = Videofile <$> (v .: "video" >>= parseJSON) <*> v .: "video"

		voice :: Object -> Parser File
		voice v = Voicerecord <$> (v .: "voice" >>= parseJSON) <*> v .: "voice"

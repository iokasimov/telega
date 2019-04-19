module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data File
	= Audiofile Text Audio
	| Videofile Text Video
	| General Text Document
	| Voicerecord Text Voice
	| Photo [Size]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v ->
		photo v <|> document v <|> audio v <|> video v <|> voice v where

		photo :: Object -> Parser File
		photo v = Photo <$> v .: "photo"

		audio :: Object -> Parser File
		audio v = Audiofile <$> (v .: "audio" >>= file_id "Audio") <*> v .: "audio"

		document :: Object -> Parser File
		document v = General <$> (v .: "document" >>= file_id "Document") <*> v .: "document"

		video :: Object -> Parser File
		video v = Videofile <$> (v .: "video" >>= file_id "Video") <*> v .: "video"

		voice :: Object -> Parser File
		voice v = Voicerecord <$> (v .: "voice" >>= file_id "Voice") <*> v .: "voice"

		file_id :: String -> Value -> Parser Text
		file_id otype = withObject otype $ \f -> f .: "file_id"

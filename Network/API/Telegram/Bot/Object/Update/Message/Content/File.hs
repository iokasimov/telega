module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data File
	= Audiofile Audio
	| Videofile Video
	| General Document
	| Voicerecord Voice
	| Photo [Size]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v -> photo v <|> document v
		<|> audio v <|> video v <|> voice v where

		audio :: Object -> Parser File
		audio v = Audiofile <$> v .: "audio"

		document :: Object -> Parser File
		document v = General <$> v .: "document"

		photo :: Object -> Parser File
		photo v = Photo <$> v .: "photo"

		video :: Object -> Parser File
		video v = Videofile <$> v .: "video"

		voice :: Object -> Parser File
		voice v = Voicerecord <$> v .: "voice"

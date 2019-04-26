module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Caption as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Duration as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<|>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

data File
	= Audiofile Audio
	| Videofile Video
	| General Document
	| Voicerecord Voice
	| Photography [Size]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v ->
		(Photography <$> v .: "photo") <|>
		(Audiofile <$> v .: "audio") <|>
		(General <$> v .: "document") <|>
		(Videofile <$> v .: "video") <|>
		(Voicerecord <$> v .: "voice")

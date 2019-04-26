module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Caption as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Duration as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.URI as Exports

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
	| Photography [Photosize]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v ->
		(Photography <$> v .: "photo") <|>
		(Audiofile <$> v .: "audio") <|>
		(General <$> v .: "document") <|>
		(Videofile <$> v .: "video") <|>
		(Voicerecord <$> v .: "voice")

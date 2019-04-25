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
import "base" Control.Applicative ((<*>), (<|>))
import "base" Data.Bool (Bool (False))
import "base" Data.Eq (Eq ((==)))
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

instance Eq File where
	Audiofile i _ == Audiofile i' _ = i == i'
	Videofile i _ == Videofile i' _ = i == i'
	General i _ == General i' _ = i == i'
	Voicerecord i _ == Voicerecord i' _ = i == i'
	Photography ss == Photography ss' = ss == ss'
	_ == _ = False

instance FromJSON File where
	parseJSON = withObject "Message" $ \v ->
		(Photography <$> v .: "photo") <|>
		(Audiofile <$> (v .: "audio" >>= parseJSON) <*> v .: "audio") <|>
		(General <$> (v .: "document" >>= parseJSON) <*> v .: "document") <|>
		(Videofile <$> (v .: "video" >>= parseJSON) <*> v .: "video") <|>
		(Voicerecord <$> (v .: "voice" >>= parseJSON) <*> v .: "voice")

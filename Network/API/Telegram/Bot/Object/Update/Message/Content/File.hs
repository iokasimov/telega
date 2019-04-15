module Network.API.Telegram.Bot.Object.Update.Message.Content.File (File (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video as Exports

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
	= Animation Text Int Int Int (Maybe Size) (Maybe Text) (Maybe Text) (Maybe Int)
	| Audiofile Audio
	| Videofile Video
	| General Document
	| Voice Text Int (Maybe Text) (Maybe Int)
	| Photo [Size]
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v -> photo v <|> document v
		<|> audio v <|> video v <|> animation v <|> voice v where

		animation :: Object -> Parser File
		animation v = v .: "animation" >>= file where

			file :: Value -> Parser File
			file = withObject "Animation" $ \f -> Animation <$> f .: "file_id"
				<*> f .: "width" <*> f .: "height" <*> f .: "duration" <*> f .:? "thumb"
				<*> f .:? "file_name" <*> f .:? "mime_type" <*> f .:? "file_size"

		audio :: Object -> Parser File
		audio v = Audiofile <$> v .: "audio"

		document :: Object -> Parser File
		document v = General <$> v .: "document"

		photo :: Object -> Parser File
		photo v = Photo <$> v .: "photo"

		video :: Object -> Parser File
		video v = Videofile <$> v .: "video"

		voice :: Object -> Parser File
		voice v = v .: "voice" >>= file where

			file :: Value -> Parser File
			file = withObject "Voice" $ \f -> Voice <$> f .: "file_id"
				<*> f .: "duration" <*> f .:? "mime_type" <*> f .:? "file_size"

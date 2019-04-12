module Network.Telegram.API.Bot.Object.Update.Message.Content.File (File (..)) where

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

import Network.Telegram.API.Bot.Object.Update.Message.Content.Size (Size)

data File
	= Animation Text Int Int Int (Maybe Size) (Maybe Text) (Maybe Text) (Maybe Int)
	| Audio Text Int (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Size)
	| Document Text (Maybe Size) (Maybe Text) (Maybe Text) (Maybe Int)
	| Video Text Int Int Int (Maybe Size) (Maybe Text) (Maybe Int)
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
		audio v = v .: "audio" >>= file where

			file :: Value -> Parser File
			file = withObject "Audio" $ \f -> Audio <$> f .: "file_id"
				<*> f .: "duration" <*> f .:? "performer" <*> f .:? "title"
				<*> f .:? "mime_type" <*> f .:? "file_size" <*> f .:? "thumb"

		document :: Object -> Parser File
		document v = v .: "document" >>= file where

			file :: Value -> Parser File
			file = withObject "Document" $ \f -> Document <$> f .: "file_id"
				<*> f .:? "thumb" <*> f .:? "file_name" <*> f .:? "mime_type" <*> f .:? "file_size"

		photo :: Object -> Parser File
		photo v = Photo <$> v .: "photo"

		video :: Object -> Parser File
		video v = v .: "video" >>= file where

			file :: Value -> Parser File
			file = withObject "Video" $ \f -> Video <$> f .: "file_id"
				<*> f .: "width" <*> f .: "height" <*> f .: "duration"
				<*> f .:?  "thumb" <*> f .:? "mime_type" <*> f .:? "file_size"

		voice :: Object -> Parser File
		voice v = v .: "voice" >>= file where

			file :: Value -> Parser File
			file = withObject "Voice" $ \f -> Voice <$> f .: "file_id"
				<*> f .: "duration" <*> f .:? "mime_type" <*> f .:? "file_size"
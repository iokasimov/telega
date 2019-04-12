module Network.Telegram.API.Bot.Object.Update.Message.Content.File (File (..), Size (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withArray, withObject, (.:), (.:?))
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
	= Animation Text Int Int Int (Maybe Int)
	| Audio Text Int
	| Document Text (Maybe Text) (Maybe Int)
	| Photo [Size]
	| Video Text Int Int Int (Maybe Int)
	| Voice Text Int (Maybe Int)
	deriving Show

instance FromJSON File where
	parseJSON = withObject "Message" $ \v -> animation v <|> audio v
		<|> document v <|> photo v <|> video v <|> voice v where

		animation :: Object -> Parser File
		animation v = v .: "animation" >>= file where

			file :: Value -> Parser File
			file = withObject "Animation" $ \f -> Animation
				<$> f .: "file_id" <*> f .: "width" <*> f .: "height"
				<*> f .: "duration" <*> f .:? "file_size"

		audio :: Object -> Parser File
		audio v = v .: "audio" >>= file where

			file :: Value -> Parser File
			file = withObject "Audio" $ \f -> Audio
				<$> f .: "file_id" <*> f .: "duration"

		document :: Object -> Parser File
		document v = v .: "document" >>= file where

			file :: Value -> Parser File
			file = withObject "Document" $ \f -> Document
				<$> f .: "file_id" <*> f .:? "file_name" <*> f .:? "file_size"

		photo :: Object -> Parser File
		photo v = Photo <$> v .: "photo"

		video :: Object -> Parser File
		video v = v .: "video" >>= file where

			file :: Value -> Parser File
			file = withObject "Video" $ \f -> Video
				<$> f .: "file_id" <*> f .: "width" <*> f .: "height"
				<*> f .: "duration" <*> f .:? "file_size"

		voice :: Object -> Parser File
		voice v = v .: "voice" >>= file where

			file :: Value -> Parser File
			file = withObject "Voice" $ \f -> Voice
				<$> f .: "file_id" <*> f .: "duration" <*> f .:? "file_size"

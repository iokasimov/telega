module Network.Telegram.API.Bot.Object.Chat (Chat (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

data Chat
	= Private Int64
	| Group Int64 Text
	| Supergroup Int64 Text
	| Channel Int64 Text
	deriving Show

instance FromJSON Chat where
	parseJSON (Object v) = v .: "type" >>= \case
		("private" :: Text) -> Private <$> v .: "id"
		("group" :: Text) -> Group <$> v .: "id" <*> v .: "title"
		("supergroup" :: Text) -> Supergroup <$> v .: "id" <*> v .: "title"
		("channel" :: Text) -> Channel <$> v .: "id" <*> v .: "title"

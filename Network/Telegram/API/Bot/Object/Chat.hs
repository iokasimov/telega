module Network.Telegram.API.Bot.Object.Chat (Chat (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Property.Identifiable
	(Identifiable (identificator), Identificator)

data Chat
	= Private Int64
	| Group Int64 Text
	| Supergroup Int64 Text
	| Channel Int64 Text
	deriving Show

type instance Identificator Chat = Int64

instance Identifiable Chat where
	identificator (Private i) = i
	identificator (Group i _) = i
	identificator (Supergroup i _) = i
	identificator (Channel i _) = i

instance FromJSON Chat where
	parseJSON = withObject "Chat" $ \v -> v .: "type" >>= \case
		("private" :: Text) -> Private <$> v .: "id"
		("group" :: Text) -> Group <$> v .: "id" <*> v .: "title"
		("supergroup" :: Text) -> Supergroup <$> v .: "id" <*> v .: "title"
		("channel" :: Text) -> Channel <$> v .: "id" <*> v .: "title"
		_ -> fail "Type of chat is not defined"

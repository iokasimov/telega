module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, identificator))

data Origin
	= Private Int64 Sender
	| Group Int64 Text Sender
	| Supergroup Int64 Text Sender
	| Channel Int64 Text
	deriving Show

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \v -> v .: "chat" >>= chat v where

		chat :: Object -> Value -> Parser Origin
		chat v = withObject "Origin" $ \c -> c .: "type" >>= \case
			("private" :: Text) -> Private <$> c .: "id" <*> v .: "from"
			("group" :: Text) -> Group <$> c .: "id" <*> c .: "title" <*> v .: "from"
			("supergroup" :: Text) -> Supergroup <$> c .: "id" <*> c .: "title" <*> v .: "from"
			("channel" :: Text) -> Channel <$> c .: "id" <*> c .: "title"
			_ -> fail "Type of chat is not defined"

instance Identifiable Origin where
	type instance Identificator Origin = Int64
	identificator (Private i _) = i
	identificator (Group i _ _) = i
	identificator (Supergroup i _ _) = i
	identificator (Channel i _) = i

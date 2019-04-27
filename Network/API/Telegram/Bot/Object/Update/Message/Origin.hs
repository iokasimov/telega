module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Object.Update.Moving.Group (Group)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Origin
	= Private Int64 Sender
	| Group Group Sender
	| Channel Int64 Title
	deriving Show

instance Eq Origin where
	o == o' = ident o == ident o'

instance Identifiable Origin where
	type Identificator Origin = Int64
	ident (Private i _) = i
	ident (Group g _) = ident g
	ident (Channel i _) = i

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \v -> v .: "chat" >>= chat v where

		chat :: Object -> Value -> Parser Origin
		chat v = withObject "Origin" $ \c -> c .: "type" >>= \case
			("private" :: Text) -> Private <$> c .: "id" <*> v .: "from"
			("channel" :: Text) -> Channel <$> c .: "id" <*> c .: "title"
			_ -> parseJSON (Object v)

module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)
import Network.API.Telegram.Bot.Object.Chat.Conversation (Conversation)
import Network.API.Telegram.Bot.Object.Chat.Group (Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Origin
	= Private Conversation Sender
	| Group Group Sender
	| Channel Int64 Title
	deriving Show

instance Eq Origin where
	o == o' = ident o == ident o'

instance Identifiable Origin where
	type Identificator Origin = Int64
	ident (Private c _) = ident c
	ident (Group g _) = ident g
	ident (Channel i _) = i

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \msg -> msg .: "chat" >>= chat msg where

		chat :: Object -> Value -> Parser Origin
		chat msg = withObject "Origin" $ \c -> c .: "type" >>= \case
			("channel" :: Text) -> Channel <$> c .: "id" <*> c .: "title"
			_ -> conversation <|> group where

				conversation, group :: Parser Origin
				conversation = Private <$> parseJSON (Object c) <*> msg .: "from"
				group = Group <$> parseJSON (Object c) <*> msg .: "from"

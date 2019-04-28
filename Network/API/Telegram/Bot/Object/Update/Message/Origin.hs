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

import Network.API.Telegram.Bot.Object.Chat.Channel (Channel)
import Network.API.Telegram.Bot.Object.Chat.Conversation (Conversation)
import Network.API.Telegram.Bot.Object.Chat.Group (Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Origin = Private Conversation Sender | Group Group Sender | Blog Channel deriving Show

instance Eq Origin where
	o == o' = ident o == ident o'

instance Identifiable Origin where
	type Identificator Origin = Int64
	ident (Private c _) = ident c
	ident (Group g _) = ident g
	ident (Blog c) = ident c

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \msg -> msg .: "chat" >>= origin msg where

		origin :: Object -> Value -> Parser Origin
		origin msg = withObject "Origin" $ \chat -> channel chat <|> conversation chat <|> group chat where

			channel, conversation, group :: Object -> Parser Origin
			channel chat = Blog <$> parseJSON (Object chat)
			conversation chat = Private <$> parseJSON (Object chat) <*> msg .: "from"
			group chat = Group <$> parseJSON (Object chat) <*> msg .: "from"

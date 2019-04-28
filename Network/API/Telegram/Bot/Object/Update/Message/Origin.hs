module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Chat (Chat, ID, Channel, Conversation, Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Origin = Private Conversation Sender | Group Group Sender | Blog Channel deriving Show

instance Accessible (ID Chat) Origin where
	access f (Private conversation sender) = flip Private sender <$> access f conversation
	access f (Group group sender) = flip Group sender <$> access f group
	access f (Blog channel) = Blog <$> access f channel

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

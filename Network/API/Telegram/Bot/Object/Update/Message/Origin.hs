module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Chat (Chat, Channel, Conversation, Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Origin = Private Conversation Sender | Group Group Sender | Blog Channel deriving Show

instance Identifiable Origin where
	type Identificator Origin = Int64
	ident (Private c _) = ident c
	ident (Group g _) = ident g
	ident (Blog c) = ident c

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \msg -> msg .: "chat" >>= \chat ->
		(Group <$> parseJSON chat <*> msg .: "from") <|>
		(Private <$> parseJSON chat <*> msg .: "from") <|>
		(Blog <$> parseJSON chat)

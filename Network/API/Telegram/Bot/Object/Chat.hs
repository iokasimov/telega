module Network.API.Telegram.Bot.Object.Chat (module Exports, Chat, Leave (..), ID (CHAT)) where

import Network.API.Telegram.Bot.Object.Chat.Group as Exports
import Network.API.Telegram.Bot.Object.Chat.Channel as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Eq (Eq)
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)

import Network.API.Telegram.Bot.Property
	(Persistable (Payload, Returning, payload, endpoint), ID)
import Network.API.Telegram.Bot.Utils (field)

data Chat

data Leave a = Leave (ID Chat)

instance Persistable (Leave Group) where
	type Payload (Leave Group) = Leave Group
	type Returning (Leave Group) = ()
	payload (Leave chat_id) = field "chat_id" chat_id
	endpoint _ = "leaveChat"

instance Persistable (Leave Channel) where
	type Payload (Leave Channel) = Leave Channel
	type Returning (Leave Channel) = ()
	payload (Leave chat_id) = field "chat_id" chat_id
	endpoint _ = "leaveChat"

data instance ID Chat = CHAT Int64

deriving instance Eq (ID Chat)

instance FromJSON (ID Chat) where
	parseJSON o = CHAT <$> parseJSON o

instance ToJSON (ID Chat) where
	toJSON (CHAT i) = toJSON i

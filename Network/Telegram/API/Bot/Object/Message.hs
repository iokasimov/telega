module Network.Telegram.API.Bot.Object.Message
	(Message (..), Initial, Marking) where

import "aeson" Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.=))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Capacity.Postable
	(Initial, Postable (initial_value, post_endpoint))
import Network.Telegram.API.Bot.Capacity.Purgeable
	(Marking, Purgeable (marking_value, purge_endpoint))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Keyboard (Keyboard)

data Message = Message Int Chat From Text deriving Show

type instance Initial Message = (Int64, Text, Maybe Keyboard)
type instance Marking Message = (Int64, Text)

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		Message <$> v .: "message_id" <*> v .: "chat"
			<*> v .: "from" <*> v .: "text"

instance Postable Message where
	initial_value (chat_id, text, Nothing) =
		object ["chat_id" .= chat_id, "text" .= text]
	initial_value (chat_id, text, Just kb) =
		object ["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	post_endpoint _ = "sendMessage"

instance Purgeable Message where
	marking_value (chat_id, text) =
		object ["chat_id" .= chat_id, "text" .= text]
	purge_endpoint _ = "deleteMessage"

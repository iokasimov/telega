module Network.Telegram.API.Bot.Object.Notification (Notification (..)) where

import "aeson" Data.Aeson (object, (.=))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Capacity.Droppable
	(Drop, Droppable (drop_value, drop_endpoint))

data Notification = Notification Text Text

type instance Drop Notification = (Text, Text)

instance Droppable Notification where
	drop_value (cbq_id, text) =
		object ["callback_query_id" .= cbq_id, "text" .= text]
	drop_endpoint _ = "answerCallbackQuery"

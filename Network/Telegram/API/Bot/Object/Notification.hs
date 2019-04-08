module Network.Telegram.API.Bot.Object.Notification (Notification) where

import "aeson" Data.Aeson (object, (.=))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Endpoint (Endpoint (payload, endpoint), Payload, Drop)
import Network.Telegram.API.Bot.Capacity.Droppable
	(Drop', Droppable (drop_value, drop_endpoint))

data Notification

type instance Drop' Notification = (Text, Text)

instance Droppable Notification where
	drop_value (cbq_id, text) = object
		["callback_query_id" .= cbq_id, "text" .= text]
	drop_endpoint _ = "answerCallbackQuery"

type instance Payload (Drop Notification) = (Text, Text)

instance Endpoint (Drop Notification) where
	payload (cbq_id, text) = object
		["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"

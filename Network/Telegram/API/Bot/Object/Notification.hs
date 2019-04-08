module Network.Telegram.API.Bot.Object.Notification (Notification, Payload) where

import "aeson" Data.Aeson (object, (.=))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Endpoint (Endpoint (payload, endpoint), Payload, Post)

data Notification

type instance Payload (Post Notification) = (Text, Text)

instance Endpoint (Post Notification) where
	payload (cbq_id, text) = object
		["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"

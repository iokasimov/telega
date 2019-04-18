module Network.API.Telegram.Bot.Object.Update.Callback.Notification (Notification) where

import "aeson" Data.Aeson (object, (.=))
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

-- import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint), Capacity (Post))

data Notification

-- instance Persistable 'Post Notification where
-- 	type instance Payload 'Post Notification = Tagged ('Post Notification) (Text, Text)
-- 	payload (untag -> (cbq_id, text)) = object ["callback_query_id" .= cbq_id, "text" .= text]
-- 	endpoint _ = "answerCallbackQuery"

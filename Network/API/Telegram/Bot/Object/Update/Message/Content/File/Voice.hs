module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice (Voice (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, object, withObject, (.:), (.:?), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Persistable
	( Persistable (Payload, payload, endpoint), Capacity (Send)
	, Inform (Notify, Silently), Way (Directly, Replying))

data Voice = Voice Text Int (Maybe Text) (Maybe Int) deriving Show

instance FromJSON Voice where
	parseJSON = withObject "Voice" $ \v -> Voice <$> v .: "file_id"
		<*> v .: "duration" <*> v .:? "mime_type" <*> v .:? "file_size"

instance Persistable ('Send 'Notify 'Directly) Voice where
	type instance Payload ('Send 'Notify 'Directly) Voice
		= Tagged ('Send 'Notify 'Directly Voice)
			(Int64, Text, Maybe Text, Maybe Int)
	payload (untag -> (chat_id, voice, caption, duration)) = object
		["chat_id" .= chat_id, "voice" .= voice, "caption" .= caption,
			"duration" .= duration, "disable_notification" .= False]
	endpoint _ = "sendVoice"

instance Persistable ('Send 'Silently 'Directly) Voice where
	type instance Payload ('Send 'Silently 'Directly) Voice
		= Tagged ('Send 'Silently 'Directly Voice)
			(Int64, Text, Maybe Text, Maybe Int)
	payload (untag -> (chat_id, voice, caption, duration)) = object
		 ["chat_id" .= chat_id, "voice" .= voice, "caption" .= caption,
		 	"duration" .= duration, "disable_notification" .= True]
	endpoint _ = "sendVoice"

instance Persistable ('Send 'Notify 'Replying) Voice where
	type instance Payload ('Send 'Notify 'Replying) Voice
		= Tagged ('Send 'Notify 'Replying Voice)
			(Int64, Int, Text, Maybe Text, Maybe Int)
	payload (untag -> (chat_id, reply_to_message_id, voice, caption, duration)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "voice" .= voice,
			"caption" .= caption, "duration" .= duration, "disable_notification" .= False]
	endpoint _ = "sendVoice"

instance Persistable ('Send 'Silently 'Replying) Voice where
	type instance Payload ('Send 'Silently 'Replying) Voice
		= Tagged ('Send 'Silently 'Replying Voice)
			(Int64, Int, Text, Maybe Text, Maybe Int)
	payload (untag -> (chat_id, reply_to_message_id, voice, caption, duration)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "voice" .= voice,
			"caption" .= caption, "duration" .= duration, "disable_notification" .= True]
	endpoint _ = "sendVoice"

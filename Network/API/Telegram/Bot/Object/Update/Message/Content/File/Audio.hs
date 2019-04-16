module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Audio (Audio (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.:?), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size)
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint)
	, Capacity (Send), Inform (Notify, Silently), Way (Directly, Replying))

data Audio = Audio Text Int (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Size)
	deriving Show

instance FromJSON Audio where
	parseJSON = withObject "Audio" $ \v -> Audio <$> v .: "file_id"
		<*> v .: "duration" <*> v .:? "performer" <*> v .:? "title"
		<*> v .:? "mime_type" <*> v .:? "file_size" <*> v .:? "thumb"

instance Persistable ('Send 'Notify 'Directly) Audio where
	type instance Payload ('Send 'Notify 'Directly) Audio
		= Tagged ('Send 'Notify 'Directly Audio)
			(Int64, Text, Text, Int, Text, Text, Maybe Text)
	payload (untag -> (chat_id, audio, caption, duration, performer, title, thumb)) = object
		["chat_id" .= chat_id, "audio" .= audio, "caption" .= caption, "duration" .= duration,
			"performer" .= performer, "title" .= title, "thumb" .= thumb, "disable_notification" .= False]
	endpoint _ = "sendAudio"

instance Persistable ('Send 'Silently 'Directly) Audio where
	type instance Payload ('Send 'Silently 'Directly) Audio
		= Tagged ('Send 'Silently 'Directly Audio)
			(Int64, Text, Text, Int, Text, Text, Maybe Text)
	payload (untag -> (chat_id, audio, caption, duration, performer, title, thumb)) = object
		["chat_id" .= chat_id, "audio" .= audio, "caption" .= caption, "duration" .= duration,
			"performer" .= performer, "title" .= title, "thumb" .= thumb, "disable_notification" .= True]
	endpoint _ = "sendAudio"

instance Persistable ('Send 'Notify 'Replying) Audio where
	type instance Payload ('Send 'Notify 'Replying) Audio
		= Tagged ('Send 'Notify 'Replying Audio)
			(Int64, Int, Text, Text, Int, Text, Text, Maybe Text)
	payload (untag -> (chat_id, reply_to_message_id, audio, caption, duration, performer, title, thumb)) =
		object ["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "audio" .= audio,
			"caption" .= caption, "duration" .= duration, "performer" .= performer, "title" .= title,
				"thumb" .= thumb, "disable_notification" .= False]
	endpoint _ = "sendAudio"

instance Persistable ('Send 'Silently 'Replying) Audio where
	type instance Payload ('Send 'Silently 'Replying) Audio
		= Tagged ('Send 'Silently 'Replying Audio)
			(Int64, Int, Text, Text, Int, Text, Text, Maybe Text)
	payload (untag -> (chat_id, reply_to_message_id, audio, caption, duration, performer, title, thumb)) =
		object ["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "audio" .= audio,
			"caption" .= caption, "duration" .= duration, "performer" .= performer, "title" .= title,
				"thumb" .= thumb, "disable_notification" .= True]
	endpoint _ = "sendAudio"

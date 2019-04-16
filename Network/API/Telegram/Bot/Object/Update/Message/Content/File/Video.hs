module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Video (Video (..)) where

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

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size)
import Network.API.Telegram.Bot.Property.Persistable
	( Persistable (Payload, payload, endpoint), Capacity (Send)
	, Inform (Notify, Silently), Way (Directly, Replying))

data Video = Video Text Int Int Int (Maybe Size) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Video where
	parseJSON = withObject "Video" $ \v -> Video <$> v .: "file_id"
		<*> v .: "width" <*> v .: "height" <*> v .: "duration"
		<*> v .:?  "thumb" <*> v .:? "mime_type" <*> v .:? "file_size"

instance Persistable ('Send 'Notify 'Directly) Video where
	type instance Payload ('Send 'Notify 'Directly) Video = Tagged ('Send 'Notify 'Directly Video)
		(Int64, Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text, Maybe Text, Bool)
	payload (untag -> (chat_id, video, duration, width, height, thumb, caption, support_streaming)) = object
		["chat_id" .= chat_id, "video" .= video, "duration" .= duration, "width" .= width, "height" .= height,
			"thumb" .= thumb, "caption" .= caption, "support_streaming" .= support_streaming, "disable_notification" .= False]
	endpoint _ = "sendVideo"

instance Persistable ('Send 'Silently 'Directly) Video where
	type instance Payload ('Send 'Silently 'Directly) Video = Tagged ('Send 'Silently 'Directly Video)
		(Int64, Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text, Maybe Text, Bool)
	payload (untag -> (chat_id, video, duration, width, height, thumb, caption, support_streaming)) = object
		["chat_id" .= chat_id, "video" .= video, "duration" .= duration, "width" .= width, "height" .= height,
			"thumb" .= thumb, "caption" .= caption, "support_streaming" .= support_streaming, "disable_notification" .= True]
	endpoint _ = "sendVideo"

instance Persistable ('Send 'Notify 'Replying) Video where
	type instance Payload ('Send 'Notify 'Replying) Video = Tagged ('Send 'Notify 'Replying Video)
		(Int64, Int, Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text, Maybe Text, Bool)
	payload (untag -> (chat_id, reply_to_message_id, video, duration, width, height, thumb, caption, support_streaming)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "video" .= video, "duration" .= duration, "width" .= width,
			"height" .= height,  "thumb" .= thumb, "caption" .= caption, "support_streaming" .= support_streaming, "disable_notification" .= False]
	endpoint _ = "sendVideo"

instance Persistable ('Send 'Silently 'Replying) Video where
	type instance Payload ('Send 'Silently 'Replying) Video = Tagged ('Send 'Silently 'Replying Video)
		(Int64, Int, Text, Maybe Int, Maybe Int, Maybe Int, Maybe Text, Maybe Text, Bool)
	payload (untag -> (chat_id, reply_to_message_id, video, duration, width, height, thumb, caption, support_streaming)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "video" .= video, "duration" .= duration, "width" .= width,
			"height" .= height,  "thumb" .= thumb, "caption" .= caption, "support_streaming" .= support_streaming, "disable_notification" .= True]
	endpoint _ = "sendVideo"

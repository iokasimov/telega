module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Document (Document (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, object, (.:), (.:?), (.=))
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

data Document = Document Text (Maybe Size) (Maybe Text) (Maybe Text) (Maybe Int)
	deriving Show

instance FromJSON Document where
	parseJSON = withObject "Document" $ \v -> Document <$> v .: "file_id"
		<*> v .:? "thumb" <*> v .:? "file_name" <*> v .:? "mime_type" <*> v .:? "file_size"

instance Persistable ('Send 'Notify 'Directly) Document where
	type instance Payload ('Send 'Notify 'Directly) Document
		= Tagged ('Send 'Notify 'Directly Document)
			(Int64, Text, Maybe Text, Maybe Text)
	payload (untag -> (chat_id, document, thumb, caption)) = object
		["chat_id" .= chat_id, "document" .= document, "thumb" .= thumb,
			"caption" .= caption, "disable_notification" .= False]
	endpoint _ = "sendDocument"

instance Persistable ('Send 'Silently 'Directly) Document where
	type instance Payload ('Send 'Silently 'Directly) Document
		= Tagged ('Send 'Silently 'Directly Document)
			(Int64, Text, Maybe Text, Maybe Text)
	payload (untag -> (chat_id, document, thumb, caption)) = object
		 ["chat_id" .= chat_id, "document" .= document, "thumb" .= thumb,
			"caption" .= caption, "disable_notification" .= True]
	endpoint _ = "sendDocument"

instance Persistable ('Send 'Notify 'Replying) Document where
	type instance Payload ('Send 'Notify 'Replying) Document
		= Tagged ('Send 'Notify 'Replying Document)
			(Int64, Int, Text, Maybe Text, Maybe Text)
	payload (untag -> (chat_id, reply_to_message_id, document, thumb, caption)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "document" .= document,
			"thumb" .= thumb, "caption" .= caption, "disable_notification" .= False]
	endpoint _ = "sendDocument"

instance Persistable ('Send 'Silently 'Replying) Document where
	type instance Payload ('Send 'Silently 'Replying) Document
		= Tagged ('Send 'Silently 'Replying Document)
			(Int64, Int, Text, Maybe Text, Maybe Text)
	payload (untag -> (chat_id, reply_to_message_id, document, thumb, caption)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "document" .= document,
			"thumb" .= thumb, "caption" .= caption, "disable_notification" .= True]
	endpoint _ = "sendDocument"

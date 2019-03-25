module Network.Telegram.API.Bot.Object.Message (Message (..), Payload (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.=))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Capacity.Postable (Postable (..))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Message = Message Int Chat From Text
	deriving Show

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		Message <$> v .: "message_id" <*> v .: "chat"
			<*> v .: "from" <*> v .: "text"

instance Postable Message where
	data Payload Message = Msg Int64 Text
	payload (Msg chat_id text) = object ["chat_id" .= chat_id, "text" .= text]
	endpoint _ = "sendMessage"

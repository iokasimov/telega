module Network.Telegram.API.Bot.Object.Message (Message) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Message = Message Int Chat From Text
	deriving Show

instance FromJSON Message where
	parseJSON (Object v) = Message
		<$> v .: "message_id"
		<*> v .: "chat"
		<*> v .: "from"
		<*> v .: "text"

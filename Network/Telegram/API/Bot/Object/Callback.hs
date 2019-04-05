module Network.Telegram.API.Bot.Object.Callback (Callback (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Has (Has (focus))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Message (Message)

data Callback = Datatext Text Message Text deriving Show

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"

instance Has Callback Chat where
	focus f (Datatext cq_id msg dttxt) = flip (Datatext cq_id) dttxt <$> focus f msg

instance Has Callback From where
	focus f (Datatext cq_id msg dttxt) = flip (Datatext cq_id) dttxt <$> focus f msg

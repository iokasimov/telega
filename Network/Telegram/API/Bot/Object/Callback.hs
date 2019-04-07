module Network.Telegram.API.Bot.Object.Callback (Callback (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Access (Access (access))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Message (Message)

data Callback = Datatext Text Message Text deriving Show

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"

instance Access Chat Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Access From Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

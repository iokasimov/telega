module Network.API.Telegram.Bot.Object.Update.Callback
	(module Exports, Callback (..), Trigger (..), ID (CB)) where

import Network.API.Telegram.Bot.Object.Update.Callback.Notification as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Chat (Chat)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Object.Update.Message (Message)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident), ID)
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, Returning, payload, endpoint))
import Network.API.Telegram.Bot.Utils (field)

data Callback = Datatext (ID Callback) Sender Message Text deriving Show

instance Eq Callback where
	c == c' = ident c == ident c'

instance Accessible Sender Callback where
	access f (Datatext cq_id sender msg dttxt) =
		(\sender' -> Datatext cq_id sender' msg dttxt) <$> f sender

instance Accessible Message Callback where
	access f (Datatext cq_id sender msg dttxt) =
		(\msg' -> Datatext cq_id sender msg' dttxt) <$> f msg

instance Accessible (ID Chat) Callback where
	access f (Datatext cq_id sender msg dttxt) =
		flip (Datatext cq_id sender) dttxt <$> access f msg

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "from"
			<*> v .: "message" <*> v .: "data"

instance Identifiable Callback where
	type Identificator Callback = ID Callback
	ident (Datatext i _ _ _) = i

data Trigger a = Trigger (ID Callback) Text

instance Persistable (Trigger Notification) where
	type Payload (Trigger Notification) = Trigger Notification
	type Returning (Trigger Notification) = ()
	payload (Trigger cbq_id text) = field "callback_query_id" cbq_id <> field "text" text
	endpoint _ = "answerCallbackQuery"

data instance ID Callback = CB Text

deriving instance Eq (ID Callback)
deriving instance Show (ID Callback)

instance FromJSON (ID Callback) where parseJSON o = CB <$> parseJSON o
instance ToJSON (ID Callback) where toJSON (CB i) = toJSON i

module Network.API.Telegram.Bot.Object.Update.Callback
	(module Exports, Callback (..), Trigger (..)) where

import Network.API.Telegram.Bot.Object.Update.Callback.Notification as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
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

data Callback = Datatext Text Sender Message Text deriving Show

instance Eq Callback where
	c == c' = ident c == ident c'

instance Accessible Sender Callback where
	access f (Datatext cq_id sender msg dttxt) =
		(\sender' -> Datatext cq_id sender' msg dttxt) <$> f sender

instance Accessible Message Callback where
	access f (Datatext cq_id sender msg dttxt) =
		(\msg' -> Datatext cq_id sender msg' dttxt) <$> f msg

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "from"
			<*> v .: "message" <*> v .: "data"

instance Identifiable Callback where
	type Identificator Callback = Text
	ident (Datatext i _ _ _) = i

data Trigger a = Trigger Text Text

instance Persistable (Trigger Notification) where
	type Payload (Trigger Notification) = Trigger Notification
	type Returning (Trigger Notification) = ()
	payload (Trigger cbq_id text) = field "text" text <> field "callback_query_id" cbq_id
	endpoint _ = "answerCallbackQuery"

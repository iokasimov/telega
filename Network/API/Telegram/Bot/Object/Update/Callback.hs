module Network.API.Telegram.Bot.Object.Update.Callback
	(module Exports, Callback (..), Trigger (..)) where

import Network.API.Telegram.Bot.Object.Update.Callback.Notification as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message (Message)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, Returning, payload, endpoint))
import Network.API.Telegram.Bot.Utils (field)

data Callback = Datatext Text Message Text deriving Show

instance Accessible Origin Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"

instance Identifiable Callback where
	type Identificator Callback = Text
	ident (Datatext i _ _) = i

data Trigger a = Trigger Text Text

instance Persistable (Trigger Notification) where
	type Payload (Trigger Notification) = Trigger Notification
	type Returning (Trigger Notification) = ()
	payload (Trigger cbq_id text) = field "text" text <> field "callback_query_id" cbq_id
	endpoint _ = "answerCallbackQuery"

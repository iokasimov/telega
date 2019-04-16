module Network.API.Telegram.Bot.Object.Update.Callback (Callback (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Callback.Notification as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message (Message)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Callback = Datatext Text Message Text deriving Show

instance Accessible Origin Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"

instance Identifiable Callback where
	type instance Identificator Callback = Text
	ident (Datatext i _ _) = i

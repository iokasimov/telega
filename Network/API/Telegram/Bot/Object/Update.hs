module Network.API.Telegram.Bot.Object.Update (module Exports, Update (..)) where

import Network.API.Telegram.Bot.Object.Update.Callback as Exports
import Network.API.Telegram.Bot.Object.Update.Message as Exports
import Network.API.Telegram.Bot.Object.Update.Moving as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Chat (Chat, ID)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Update
	= Query (ID Update) Callback
	| Membership (ID Update) Moving
	| Incoming (ID Update) Message
	deriving Show

instance Accessible (ID Chat) Update where
	access f (Query i callback) = Query i <$> access f callback
	access f (Membership i moving) = Membership i <$> access f moving
	access f (Incoming i message) = Incoming i <$> access f message

instance Identifiable Update where
	type Identificator Update = ID Update
	ident (Query i _) = i
	ident (Membership i _) = i
	ident (Incoming i _) = i

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		(Query <$> v .: "update_id" <*> v .: "callback_query") <|>
		(Membership <$> v .: "update_id" <*> v .: "message") <|>
		(Incoming <$> v .: "update_id" <*> v .: "message")

data instance ID Update = UPD Int

deriving instance Eq (ID Update)
deriving instance Show (ID Update)

instance FromJSON (ID Update) where parseJSON o = UPD <$> parseJSON o
instance ToJSON (ID Update) where toJSON (UPD i) = toJSON i

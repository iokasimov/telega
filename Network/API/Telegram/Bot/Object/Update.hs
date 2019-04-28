module Network.API.Telegram.Bot.Object.Update (module Exports, Update (..), ID (..)) where

import Network.API.Telegram.Bot.Object.Update.Callback as Exports
import Network.API.Telegram.Bot.Object.Update.Message as Exports
import Network.API.Telegram.Bot.Object.Update.Moving as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident), ID)

data Update
	= Query Int Callback
	| Membership Int Moving
	| Incoming Int Message
	deriving Show

instance Eq Update where
	u == u' = ident u == ident u'

instance Identifiable Update where
	type Identificator Update = Int
	ident (Query i _) = i
	ident (Membership i _) = i
	ident (Incoming i _) = i

data instance ID Update = UPD Int

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		(Query <$> v .: "update_id" <*> v .: "callback_query") <|>
		(Membership <$> v .: "update_id" <*> v .: "message") <|>
		(Incoming <$> v .: "update_id" <*> v .: "message")

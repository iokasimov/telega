module Network.Telegram.API.Bot.Object.Update (Update (..), module Exports) where

import Network.Telegram.API.Bot.Object.Update.Callback as Exports
import Network.Telegram.API.Bot.Object.Update.Message as Exports
import Network.Telegram.API.Bot.Object.Update.Moving as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Update.Callback (Callback)
import Network.Telegram.API.Bot.Object.Update.Message (Message)
import Network.Telegram.API.Bot.Object.Update.Moving (Moving)

data Update
	= Query Int Callback
	| Membership Int Moving
	| Incoming Int Message
	deriving Show

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		query v <|> membership v <|> incoming v where

		query :: Object -> Parser Update
		query v = Query <$> v .: "update_id" <*> v .: "callback_query"

		membership :: Object -> Parser Update
		membership v = Membership <$> v .: "update_id" <*> v .: "message"

		incoming :: Object -> Parser Update
		incoming v = Incoming <$> v .: "update_id" <*> v .: "message"

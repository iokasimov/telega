module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Callback (Callback)
import Network.Telegram.API.Bot.Object.Moving (Moving)
import Network.Telegram.API.Bot.Object.Message (Message)

data Update
	= Query Int Callback
	| Membership Int Moving
	-- | Forward Int Message
	| Incoming Int Message
	deriving Show

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		query v <|> membership v <|> incoming v where

		query :: Object -> Parser Update
		query v = Query <$> v .: "update_id" <*> v .: "callback_query"

		membership :: Object -> Parser Update
		membership v = Membership <$> v .: "update_id" <*> v .: "message"

		-- forward :: Object -> Parser Update
		-- forward v = Forward <$> v .: "update_id" <*> (v .: "message" >>= forward_message) where
		--
		-- 	forward_message = withObject "Forwarded message" $ \o ->
		-- 		o .: "forward_from_message_id" <>

		incoming :: Object -> Parser Update
		incoming v = Incoming <$> v .: "update_id" <*> v .: "message"

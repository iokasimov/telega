module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))

import Network.Telegram.API.Bot.Object.Callback (Callback)
import Network.Telegram.API.Bot.Object.Message (Message)

data Update
	= Incoming Int Message
	| Query Int Callback
	deriving Show

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		(Query <$> v .: "update_id" <*> query v) <|>
		(Incoming <$> v .: "update_id" <*> incoming v) where

		incoming :: Object -> Parser Message
		incoming v = v .: "message"

		query :: Object -> Parser Callback
		query v = v .: "callback_query"

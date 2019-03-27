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
	parseJSON = withObject "Update" $ \v -> query v <|> incoming v where

		query :: Object -> Parser Update
		query v = Query <$> v .: "update_id" <*> v .: "callback_query"

		incoming :: Object -> Parser Update
		incoming v = Incoming <$> v .: "update_id" <*> v .: "message"

module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))

import Network.Telegram.API.Bot.Object.Callback (Callback)
import Network.Telegram.API.Bot.Object.Message (Message)
import Network.Telegram.API.Bot.Property.Identifiable
	(Identifiable (identificator), Identificator)

data Update
	= Incoming Int Message
	| Query Int Callback
	deriving Show

type instance Identificator Update = Int

instance Identifiable Update where
	identificator (Incoming i _) = i
	identificator (Query i _) = i

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v -> query v <|> incoming v where

		query :: Object -> Parser Update
		query v = Query <$> v .: "update_id" <*> v .: "callback_query"

		incoming :: Object -> Parser Update
		incoming v = Incoming <$> v .: "update_id" <*> v .: "message"

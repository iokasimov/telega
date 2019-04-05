module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))

import Network.Telegram.API.Bot.Has (Has (focus))
import Network.Telegram.API.Bot.Object.Callback (Callback)
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.Member (Member)
import Network.Telegram.API.Bot.Object.Message (Message)
import Network.Telegram.API.Bot.Property.Identifiable
	(Identifiable (identificator), Identificator)

data Update
	= Query Int Callback
	| Membership Int Member
	| Incoming Int Message
	deriving Show

type instance Identificator Update = Int

instance Identifiable Update where
	identificator (Query i _) = i
	identificator (Membership i _) = i
	identificator (Incoming i _) = i

instance FromJSON Update where
	parseJSON = withObject "Update" $ \v ->
		query v <|> membership v <|> incoming v where

		query :: Object -> Parser Update
		query v = Query <$> v .: "update_id" <*> v .: "callback_query"

		membership :: Object -> Parser Update
		membership v = Membership <$> v .: "update_id" <*> v .: "message"

		incoming :: Object -> Parser Update
		incoming v = Incoming <$> v .: "update_id" <*> v .: "message"

instance Has Update Chat where
	focus f (Incoming upd_id msg) = Incoming upd_id <$> focus f msg
	focus f (Query upd_id cb) = Query upd_id <$> focus f cb
	focus f (Membership upd_id mmb) = Membership upd_id <$> focus f mmb

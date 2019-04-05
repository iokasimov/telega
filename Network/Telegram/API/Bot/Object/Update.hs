module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))
import "lens" Control.Lens (Lens')

import Network.Telegram.API.Bot.Has (Has (focus))
import Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.Member (Member (Gone, Joined))
import Network.Telegram.API.Bot.Object.Message (Message (Textual, Command))
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
	focus f (Incoming upd_id (Command msg_id chat_ from cmd)) =
		(\chat' -> Incoming upd_id (Command msg_id chat' from cmd)) <$> f chat_
	focus f (Incoming upd_id (Textual msg_id chat_ from txt)) =
		(\chat' -> Incoming upd_id (Textual msg_id chat' from txt)) <$> f chat_
	focus f (Query upd_id (Datatext cq_id (Command msg_id chat_ from cmd) dttxt)) =
		(\chat' -> Query upd_id (Datatext cq_id (Command msg_id chat' from cmd) dttxt)) <$> f chat_
	focus f (Query upd_id (Datatext cq_id (Textual msg_id chat_ from txt) dttxt)) =
		(\chat' -> Query upd_id (Datatext cq_id (Textual msg_id chat' from txt) dttxt)) <$> f chat_
	focus f (Membership upd_id (Gone chat_ users)) =
		(\chat' -> Membership upd_id (Gone chat' users)) <$> f chat_
	focus f (Membership upd_id (Joined chat_ users)) =
		(\chat' -> Membership upd_id (Joined chat' users)) <$> f chat_

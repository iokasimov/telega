module Network.Telegram.API.Bot.Property.Accessible (Accessible (..)) where

import "base" Data.Function (flip)
import "base" Data.Functor ((<$>))
import "lens" Control.Lens (Lens')

import Network.Telegram.API.Bot.Object (Object)
import Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Message (Message (Textual, Command))
import Network.Telegram.API.Bot.Object.Moving (Moving (Gone, Joined))
import Network.Telegram.API.Bot.Object.Update (Update (Query, Membership, Incoming))

class Object source => Accessible target source where
	access :: Lens' source target

instance Accessible Chat Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Accessible From Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Accessible Chat Message where
	access f (Textual msg_id chat from txt) = (\chat' -> Textual msg_id chat' from txt) <$> f chat
	access f (Command msg_id chat from cmd) = (\chat' -> Command msg_id chat' from cmd) <$> f chat

instance Accessible From Message where
	access f (Textual msg_id chat from txt) = (\from' -> Textual msg_id chat from' txt) <$> f from
	access f (Command msg_id chat from cmd) = (\from' -> Command msg_id chat from' cmd) <$> f from

instance Accessible Chat Moving where
	access f (Gone chat from) = (\chat' -> Gone chat' from) <$> f chat
	access f (Joined chat froms) = (\chat' -> Joined chat' froms) <$> f chat

instance Accessible Chat Update where
	access f (Incoming upd_id msg) = Incoming upd_id <$> access f msg
	access f (Query upd_id cb) = Query upd_id <$> access f cb
	access f (Membership upd_id mmb) = Membership upd_id <$> access f mmb

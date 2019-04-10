module Network.Telegram.API.Bot.Property.Accessible (Accessible (..)) where

import "base" Data.Function (flip)
import "base" Data.Functor ((<$>))
import "lens" Control.Lens (Lens')

import Network.Telegram.API.Bot.Object (Object)
import Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.Content (Content)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Message (Message (Direct, Forward))
import Network.Telegram.API.Bot.Object.Moving (Moving (Gone, Joined))
import Network.Telegram.API.Bot.Object.Update (Update (Query, Membership, Incoming))

class Object source => Accessible target source where
	access :: Lens' source target

instance Accessible Chat Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Accessible Chat Message where
	access f (Direct msg_id chat from content) = (\chat' -> Direct msg_id chat' from content) <$> f chat
	access f (Forward msg_id chat from content) = (\chat' -> Forward msg_id chat' from content) <$> f chat

instance Accessible Chat Moving where
	access f (Gone chat from) = (\chat' -> Gone chat' from) <$> f chat
	access f (Joined chat froms) = (\chat' -> Joined chat' froms) <$> f chat

instance Accessible Chat Update where
	access f (Incoming upd_id msg) = Incoming upd_id <$> access f msg
	access f (Query upd_id cb) = Query upd_id <$> access f cb
	access f (Membership upd_id mmb) = Membership upd_id <$> access f mmb

instance Accessible Content Message where
	access f (Direct msg_id chat from content) = (\content' -> Direct msg_id chat from content') <$> f content
	access f (Forward msg_id chat from content) = (\content' -> Forward msg_id chat from content') <$> f content

instance Accessible From Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Accessible From Message where
	access f (Direct msg_id chat from content) = (\from' -> Direct msg_id chat from' content) <$> f from
	access f (Forward msg_id chat from content) = (\from' -> Forward msg_id chat from' content) <$> f from

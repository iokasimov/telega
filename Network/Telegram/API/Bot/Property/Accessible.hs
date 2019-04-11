module Network.Telegram.API.Bot.Property.Accessible (Accessible (..)) where

import "base" Data.Function (flip)
import "base" Data.Functor ((<$>))
import "lens" Control.Lens (Lens')

import Network.Telegram.API.Bot.Object (Object)
import Network.Telegram.API.Bot.Object.Update.Callback (Callback (Datatext))
import Network.Telegram.API.Bot.Object.Update.Message (Message (Direct, Forward, Reply))
import Network.Telegram.API.Bot.Object.Update.Message.Content (Content)
import Network.Telegram.API.Bot.Object.Update.Message.Origin (Origin)

class Object source => Accessible target source where
	access :: Lens' source target

instance Accessible Content Message where
	access f (Direct msg_id origin content) = (\content' -> Direct msg_id origin content') <$> f content
	access f (Forward msg_id origin content) = (\content' -> Forward msg_id origin content') <$> f content
	access f (Reply msg_id origin content msg) = (\content' -> Reply msg_id origin content' msg) <$> f content

instance Accessible Origin Callback where
	access f (Datatext cq_id msg dttxt) = flip
		(Datatext cq_id) dttxt <$> access f msg

instance Accessible Origin Message where
	access f (Direct msg_id origin content) = (\origin' -> Direct msg_id origin' content) <$> f origin
	access f (Forward msg_id origin content) = (\origin' -> Forward msg_id origin' content) <$> f origin
	access f (Reply msg_id origin content msg) = (\origin' -> Reply msg_id origin' content msg) <$> f origin

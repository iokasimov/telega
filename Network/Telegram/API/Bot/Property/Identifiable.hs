module Network.Telegram.API.Bot.Property.Identifiable (Identifiable (..), Identificator) where

import "base" Data.Int (Int, Int64)

import Network.Telegram.API.Bot.Object (Object)
import Network.Telegram.API.Bot.Object.Update (Update (Query, Membership, Incoming))
import Network.Telegram.API.Bot.Object.Update.Message (Message (Direct, Forward))
import Network.Telegram.API.Bot.Object.Update.Message.Origin (Origin (Private, Group, Supergroup, Channel))
import Network.Telegram.API.Bot.Object.Sender (Sender (Bot, User))

type family Identificator o = i

class Object o => Identifiable o where
	{-# MINIMAL identificator #-}
	identificator :: o -> Identificator o

type instance Identificator Message = Int
type instance Identificator Origin = Int64
type instance Identificator Sender = Int
type instance Identificator Update = Int

instance Identifiable Message where
	identificator (Direct i _ _) = i
	identificator (Forward i _ _) = i

instance Identifiable Origin where
	identificator (Private i _) = i
	identificator (Group i _ _) = i
	identificator (Supergroup i _ _) = i
	identificator (Channel i _) = i

instance Identifiable Sender where
	identificator (Bot i _ _ _ _) = i
	identificator (User i _ _ _ _) = i

instance Identifiable Update where
	identificator (Query i _) = i
	identificator (Membership i _) = i
	identificator (Incoming i _) = i

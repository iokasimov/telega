module Network.Telegram.API.Bot.Property.Identifiable (Identifiable (..), Identificator) where

import "base" Data.Int (Int)

import Network.Telegram.API.Bot.Object (Chat (..), From (..), Message (..), Update (..))

type family Identificator a = r

class Identifiable o where
	{-# MINIMAL identificator #-}
	identificator :: o -> Identificator o

type instance Identificator Chat = Int64
type instance Identificator From = Int
type instance Identificator Message = Int
type instance Identificator Update = Int

instance Identifiable Chat where
	identificator (Private i) = i
	identificator (Group i _) = i
	identificator (Supergroup i _) = i
	identificator (Channel i _) = i

instance Identifiable From where
	identificator (Bot i _ _ _ _) = i
	identificator (User i _ _ _ _) = i

instance Identifiable Message where
	identificator (Textual i _ _ _) = i
	identificator (Command i _ _ _) = i

instance Identifiable Update where
	identificator (Query i _) = i
	identificator (Membership i _) = i
	identificator (Incoming i _) = i

module Network.Telegram.API.Bot.Property.Identifiable (Identifiable (..), Identificator) where

import "base" Data.Int (Int)

import Network.Telegram.API.Bot.Object (Object, From, Message (Direct, Forward), Update (Query, Membership, Incoming))

type family Identificator o = i

class Object o => Identifiable o where
	{-# MINIMAL identificator #-}
	identificator :: o -> Identificator o

type instance Identificator From = Int
type instance Identificator Message = Int
type instance Identificator Update = Int

instance Identifiable Message where
	identificator (Direct i _ _) = i
	identificator (Forward i _ _) = i

instance Identifiable Update where
	identificator (Query i _) = i
	identificator (Membership i _) = i
	identificator (Incoming i _) = i

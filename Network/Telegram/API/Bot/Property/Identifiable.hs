module Network.Telegram.API.Bot.Property.Identifiable (Identifiable (..), Identificator) where

type family Identificator a = r

class Identifiable a where
	{-# MINIMAL identificator #-}
	identificator :: a -> Identificator a

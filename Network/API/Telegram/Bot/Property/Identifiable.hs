module Network.API.Telegram.Bot.Property.Identifiable (Identifiable (..), ID) where

class Identifiable o where
	{-# MINIMAL ident #-}
	type family Identificator o :: *
	ident :: o -> Identificator o

data family ID a :: *

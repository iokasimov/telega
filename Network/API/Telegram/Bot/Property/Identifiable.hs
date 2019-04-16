module Network.API.Telegram.Bot.Property.Identifiable (Identifiable (..)) where

class Identifiable o where
	{-# MINIMAL identificator #-}
	type family Identificator o :: *
	identificator :: o -> Identificator o

module Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (..), Marking) where

import "aeson" Data.Aeson (FromJSON, Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Internal (telegram_request)
import Network.Telegram.API.Bot.Core (Telegram)

type family Marking a = r | r -> a

class Purgeable a where
	{-# MINIMAL marking_value, purge_endpoint #-}
	marking_value :: Marking a -> Value
	purge_endpoint :: Marking a -> String

	purge :: Marking a -> Telegram e ()
	purge x = telegram_request (purge_endpoint x) (marking_value x)

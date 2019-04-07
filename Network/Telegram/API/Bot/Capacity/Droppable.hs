module Network.Telegram.API.Bot.Capacity.Droppable (Droppable (..), Drop) where

import "aeson" Data.Aeson (Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Internal (telegram_request)
import Network.Telegram.API.Bot.Core (Telegram)

type family Drop a = r | r -> a

class Droppable a where
	{-# MINIMAL drop_value, drop_endpoint #-}
	drop_value :: Drop a -> Value
	drop_endpoint :: Drop a -> String

	drop :: Drop a -> Telegram e ()
	drop x = telegram_request (drop_endpoint x) (drop_value x)

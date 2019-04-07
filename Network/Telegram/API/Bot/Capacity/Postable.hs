module Network.Telegram.API.Bot.Capacity.Postable (Postable (..), Initial) where

import "aeson" Data.Aeson (FromJSON, Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Internal (telegram_request)
import Network.Telegram.API.Bot.Core (Telegram)

type family Initial a = r | r -> a

class FromJSON a => Postable a where
	{-# MINIMAL initial_value, post_endpoint #-}
	initial_value :: Initial a -> Value
	post_endpoint :: Initial a -> String

	post :: Initial a -> Telegram e a
	post x = telegram_request (post_endpoint x) (initial_value x)

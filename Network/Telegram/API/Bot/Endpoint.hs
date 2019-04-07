module Network.Telegram.API.Bot.Endpoint (Endpoint (..), Payload, Edit (..), Post (..), Purge (..)) where

import "aeson" Data.Aeson (FromJSON, Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Core (Telegram)
import Network.Telegram.API.Bot.Internal (telegram_request)

type family Payload a = r | r -> a

newtype Edit a = Edit a
newtype Post a = Post a
newtype Purge a = Purge a

class Endpoint a where
	{-# MINIMAL value, endpoint #-}
	value :: Payload a -> Value
	endpoint :: Payload a -> String
	request :: FromJSON r => Payload a -> Telegram e r
	request x = telegram_request (endpoint x) (value x)

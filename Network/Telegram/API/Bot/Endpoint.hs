module Network.Telegram.API.Bot.Endpoint
	(Endpoint (..), Payload, Drop, Edit, Post, Purge) where

import "aeson" Data.Aeson (FromJSON, Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Core (Telegram)
import Network.Telegram.API.Bot.Internal (telegram_request)

type family Payload a = r | r -> a

data Drop a
data Edit a
data Post a
data Purge a

class Endpoint a where
	{-# MINIMAL payload, endpoint #-}
	payload :: Payload a -> Value
	endpoint :: Payload a -> String
	request :: FromJSON r => Payload a -> Telegram e r
	request x = telegram_request (endpoint x) (payload x)

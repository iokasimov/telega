module Network.Telegram.API.Bot.Capacity.Editable (Editable (..), Substitution) where

import "aeson" Data.Aeson (FromJSON, Value)
import "base" Data.String (String)

import Network.Telegram.API.Bot.Internal (telegram_request)
import Network.Telegram.API.Bot.Core (Telegram)

type family Substitution a = r | r -> a

class FromJSON a => Editable a where
	{-# MINIMAL substitution_value, edit_endpoint #-}
	substitution_value :: Substitution a -> Value
	edit_endpoint :: Substitution a -> String

	edit :: Substitution a -> Telegram e a
	edit x = telegram_request (edit_endpoint x) (substitution_value x)

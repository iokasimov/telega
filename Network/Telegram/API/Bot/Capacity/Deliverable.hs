module Network.Telegram.API.Bot.Capacity.Deliverable (Deliverable (..)) where

import "aeson" Data.Aeson (ToJSON)

import Network.Telegram.API.Bot.Core (Telegram)

class ToJSON a => Deliverable a where
	deliver :: a -> Telegram e ()

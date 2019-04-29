module Network.API.Telegram.Bot.Identifier.Chat (ID (..)) where

import "aeson" Data.Aeson (ToJSON (toJSON))
import "base" Data.Eq (Eq)
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)

import Network.API.Telegram.Bot.Object.Chat (Chat)
import Network.API.Telegram.Bot.Property (ID)

data instance ID Chat = CHAT Int64

deriving instance Eq (ID Chat)

instance ToJSON (ID Chat) where
	toJSON (CHAT i) = toJSON i

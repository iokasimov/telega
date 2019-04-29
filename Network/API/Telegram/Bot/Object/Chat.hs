module Network.API.Telegram.Bot.Object.Chat (module Exports, Chat, ID (..)) where

import Network.API.Telegram.Bot.Object.Chat.Group as Exports
import Network.API.Telegram.Bot.Object.Chat.Channel as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Eq (Eq)
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property (ID)

data Chat

data instance ID Chat = CHAT Int64

deriving instance Eq (ID Chat)
deriving instance Show (ID Chat)

instance FromJSON (ID Chat) where
	parseJSON o = CHAT <$> parseJSON o

instance ToJSON (ID Chat) where
	toJSON (CHAT i) = toJSON i

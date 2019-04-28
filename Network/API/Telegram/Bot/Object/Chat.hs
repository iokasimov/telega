module Network.API.Telegram.Bot.Object.Chat (module Exports, Chat, ID (..)) where

import Network.API.Telegram.Bot.Object.Chat.Group as Exports
import Network.API.Telegram.Bot.Object.Chat.Conversation as Exports
import Network.API.Telegram.Bot.Object.Chat.Channel as Exports

import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)

import Network.API.Telegram.Bot.Property (Accessible (access), ID)

data Chat

data instance ID Chat = CHAT Int64

instance Accessible (ID Chat) Channel where
	access f (Channel i title) = (\(CHAT i') -> Channel i' title) <$> f (CHAT i)

instance Accessible (ID Chat) Conversation where
	access f (Conversation i) = (\(CHAT i') -> Conversation i') <$> f (CHAT i)

instance Accessible (ID Chat) Group where
	access f (Basic i title) = (\(CHAT i') -> Basic i' title) <$> f (CHAT i)
	access f (Super i title description) = (\(CHAT i') -> Super i' title description) <$> f (CHAT i)

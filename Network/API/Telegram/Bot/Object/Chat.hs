module Network.API.Telegram.Bot.Object.Chat (module Exports, Chat, ID (..)) where

import Network.API.Telegram.Bot.Object.Chat.Channel as Exports
import Network.API.Telegram.Bot.Object.Chat.Conversation as Exports
import Network.API.Telegram.Bot.Object.Chat.Group as Exports

import "base" Data.Int (Int64)

import Network.API.Telegram.Bot.Property (ID)

data Chat

data instance ID Chat = CHAT Int64

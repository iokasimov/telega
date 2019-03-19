module Network.Telegram.API.Bot.Message (Message) where

import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Chat (Chat)
import Network.Telegram.API.Bot.From (From)

data Message = Message Int Chat From Text
	deriving Show

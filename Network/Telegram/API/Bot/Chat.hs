module Network.Telegram.API.Bot.Chat (Chat (..)) where

import "base" Data.Int (Int64)
import "text" Data.Text (Text)

data Chat
	= Private Int64
	| Group Int64 (Maybe Text)
	| Supergroup Int64 (Maybe Text)
	| Channel Int64 (Maybe Text)
	deriving Show

module Network.Telegram.API.Bot where

import "base" Data.Int
import "aeson" Data.Aeson
import "text" Data.Text

data Message = Message Int Chat From Text

data Chat
	= Private Int64
	| Group Int64 (Maybe Text)
	| Supergroup Int64 (Maybe Text)
	| Channel Int64 (Maybe Text)
	deriving Show

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

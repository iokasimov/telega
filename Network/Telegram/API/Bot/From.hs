module Network.Telegram.API.Bot.From (From (..)) where

import "base" Data.Int (Int64)
import "text" Data.Text (Text)

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

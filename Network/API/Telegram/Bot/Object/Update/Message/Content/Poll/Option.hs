module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option (Option (..)) where

import "base" Data.Int (Int)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Option = Option Text Int deriving Show

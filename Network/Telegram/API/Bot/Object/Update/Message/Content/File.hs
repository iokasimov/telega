module Network.Telegram.API.Bot.Object.Update.Message.Content.File (File (..)) where

import "base" Text.Show (Show)

data File = Animation | Audio | Document | Photo | Video | Voice deriving Show

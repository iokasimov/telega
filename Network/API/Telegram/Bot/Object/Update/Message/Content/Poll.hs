module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (Poll (..)) where

import "base" Data.Bool (Bool)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.Option (Option)

data Poll = Poll Text [Option] Bool deriving Show

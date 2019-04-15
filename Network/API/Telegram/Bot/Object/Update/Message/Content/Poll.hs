module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (Poll (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option as Exports

import "base" Data.Bool (Bool)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Poll = Poll Text [Option] Bool deriving Show

module Network.Telegram.API.Bot.Object.Notification (Notification (..)) where

import "base" Data.Int (Int64)
import "text" Data.Text (Text)

data Notification = Notification Int Text

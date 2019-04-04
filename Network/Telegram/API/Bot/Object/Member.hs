module Network.Telegram.API.Bot.Object.Member (Member (..)) where

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Member = Left Chat From | Joined Chat [From] deriving Show

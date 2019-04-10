module Network.Telegram.API.Bot.Object (module Exports, Object) where

import Network.Telegram.API.Bot.Object.Update as Exports
import Network.Telegram.API.Bot.Object.Notification as Exports
import Network.Telegram.API.Bot.Object.Moving as Exports
import Network.Telegram.API.Bot.Object.Message as Exports
import Network.Telegram.API.Bot.Object.Member as Exports
import Network.Telegram.API.Bot.Object.Keyboard as Exports
import Network.Telegram.API.Bot.Object.From as Exports
import Network.Telegram.API.Bot.Object.Content as Exports
import Network.Telegram.API.Bot.Object.Chat as Exports
import Network.Telegram.API.Bot.Object.Callback as Exports
import Network.Telegram.API.Bot.Object.Button as Exports

import "base" Data.Kind (Constraint)

type family Object (a :: *) :: Constraint
type instance Object Update = ()
type instance Object Notification = ()
type instance Object Moving = ()
type instance Object Message = ()
type instance Object Member = ()
type instance Object Keyboard = ()
type instance Object From = ()
type instance Object Chat = ()
type instance Object Callback = ()
type instance Object Button = ()

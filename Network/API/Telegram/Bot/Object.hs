module Network.API.Telegram.Bot.Object (module Exports, Object) where

import Network.API.Telegram.Bot.Object.Update as Exports
import Network.API.Telegram.Bot.Object.Sender as Exports
import Network.API.Telegram.Bot.Object.Member as Exports
import Network.API.Telegram.Bot.Object.Group as Exports

import "base" Data.Kind (Constraint)

type family Object (a :: *) :: Constraint
type instance Object Sender = ()
type instance Object Update = ()
type instance Object Notification = ()
type instance Object Moving = ()
type instance Object Message = ()
type instance Object Member = ()
type instance Object Keyboard = ()
type instance Object Content = ()
type instance Object Origin = ()
type instance Object Callback = ()
type instance Object Button = ()
type instance Object Info = ()
type instance Object Location = ()

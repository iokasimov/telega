module Network.Telegram.API.Bot.Object (module Exports, Object) where

import Network.Telegram.API.Bot.Object.Update as Exports
import Network.Telegram.API.Bot.Object.Sender as Exports
import Network.Telegram.API.Bot.Object.Member as Exports

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

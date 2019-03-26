module Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (..)) where

import "aeson" Data.Aeson (Value)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token))

type family Marking a = r | r -> a

class Purgeable a where
	{-# MINIMAL marking_value, purge_endpoint #-}
	marking_value :: Marking a -> Value
	purge_endpoint :: Marking a -> String
	purge :: Marking a -> Telegram e ()

module Network.Telegram.API.Bot.Property.Accessible (Accessible (..)) where

import "lens" Control.Lens (Lens')

import Network.Telegram.API.Bot.Object (Object)

class Object source => Accessible target source where
	access :: Lens' source target

module Network.API.Telegram.Bot.Property.Accessible (Accessible (..)) where

import "lens" Control.Lens (Lens')

class Accessible target source where
	access :: Lens' source target

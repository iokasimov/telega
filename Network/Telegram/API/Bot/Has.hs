module Network.Telegram.API.Bot.Has (Has (..)) where

import "lens" Control.Lens (Lens')

class Has source target where
	focus :: Lens' source target

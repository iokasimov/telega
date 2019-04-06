module Network.Telegram.API.Bot.Access (Access (..)) where

import "lens" Control.Lens (Lens')

class Access target source where
	access :: Lens' source target

module Network.Telegram.API.Bot.Capacity.Postable (Postable (..)) where

import "aeson" Data.Aeson (FromJSON, Value)
import "text" Data.Text (Text)

class FromJSON a => Postable a where
	type Payload a
	payload :: Payload a -> Value
	endpoing :: Text

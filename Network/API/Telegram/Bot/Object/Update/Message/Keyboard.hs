module Network.API.Telegram.Bot.Object.Update.Message.Keyboard (Keyboard (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Keyboard.Button as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))

data Keyboard = Inline [[Button]]

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

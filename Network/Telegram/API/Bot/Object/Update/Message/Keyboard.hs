module Network.Telegram.API.Bot.Object.Update.Message.Keyboard (Keyboard (..), module Exports) where

import Network.Telegram.API.Bot.Object.Update.Message.Keyboard.Button as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Update.Message.Keyboard.Button (Button)

data Keyboard = Inline [[Button]] deriving Show

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

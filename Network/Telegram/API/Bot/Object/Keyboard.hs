module Network.Telegram.API.Bot.Object.Keyboard (Keyboard (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Button (Button)

data Keyboard = Inline [[Button]] deriving Show

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

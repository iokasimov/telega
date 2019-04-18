module Network.API.Telegram.Bot.Object.Update.Message.Keyboard (Keyboard (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Keyboard.Button as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (singleton)

data Keyboard = Inline [[Button]] deriving Show

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

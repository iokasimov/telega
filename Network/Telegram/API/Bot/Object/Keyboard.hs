module Network.Telegram.API.Bot.Object.Keyboard (Keyboard (..), Substitution) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Capacity.Editable
	(Substitution, Editable (substitution_value, edit_endpoint))
import Network.Telegram.API.Bot.Object.Button (Button)

data Keyboard = Inline [[Button]] deriving Show

type instance Substitution Keyboard = (Int64, Int, Keyboard)

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

instance Editable Keyboard where
	substitution_value (chat_id, message_id, reply_markup) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	edit_endpoint _ = "editMessageReplyMarkup"

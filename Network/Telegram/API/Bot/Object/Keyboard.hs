module Network.Telegram.API.Bot.Object.Keyboard (Keyboard (..), Payload) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Endpoint (Endpoint (payload, endpoint), Payload, Edit)
import Network.Telegram.API.Bot.Object.Button (Button)

data Keyboard = Inline [[Button]] deriving Show

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

type instance Payload (Edit Keyboard) = (Int64, Int, Keyboard)

instance Endpoint (Edit Keyboard) where
	payload (chat_id, message_id, reply_markup) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	endpoint _ = "editMessageReplyMarkup"

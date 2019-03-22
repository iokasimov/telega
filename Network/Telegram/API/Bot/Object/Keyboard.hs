module Network.Telegram.API.Bot.Object.Keyboard (Keyboard (..), Button (..), Pressed (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON)
	, Value (Object), object, (.:), (.:?), (.=))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Alternative ((<|>)))
import "text" Data.Text (Text)

data Keyboard
	= Inline [[Button]]
	deriving Show

instance FromJSON Keyboard where
	parseJSON (Object v) = Inline
		<$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

data Button = Button Text Pressed
	deriving Show

instance FromJSON Button where
	parseJSON (Object v) = Button
		<$> v .: "text" <*> parseJSON (Object v)

instance ToJSON Button where
	toJSON (Button text (Open url)) = object
		["text" .= text, "url" .= url]
	toJSON (Button text (Callback cbd)) = object
		["text" .= text, "callback_data" .= cbd]

data Pressed
	= Open Text
	| Callback Text
	deriving Show

instance FromJSON Pressed where
	parseJSON (Object v) = (is_open <|> is_callback) >>= maybe
		(fail "Action on pressing isn't set") return where

		is_open, is_callback :: Parser (Maybe Pressed)
		is_open = (fmap . fmap) Open $ v .:? "url"
		is_callback = (fmap . fmap) Callback $ v .:? "callback_data"

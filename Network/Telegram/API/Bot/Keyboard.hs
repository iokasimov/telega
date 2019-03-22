module Network.Telegram.API.Bot.Keyboard (Button (..), Keyboard (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), (.:), (.:?))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Alternative ((<|>)))
import "base" Data.Functor ((<&>))
import "text" Data.Text (Text)

data Keyboard
	= Inline [[Button]]

instance FromJSON Keyboard where
	parseJSON (Object v) = Inline <$> v .: "inline_keyboard"

data Button = Button Text Pressed
	deriving Show

instance FromJSON Button where
	parseJSON (Object v) = Button <$> v .: "text" <*> parseJSON (Object v)

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

-- instance ToJSON
-- 	toJSON (Person name age) =
-- 		object ["name" .= name, "age" .= age]

module Network.Telegram.API.Bot.Object.Button (Button (..), Pressed (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON)
	, Object, Value (Object), object, withObject, (.:), (.:?), (.=))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Alternative ((<|>)))
import "text" Data.Text (Text)

data Button = Button Text Pressed deriving Show

instance FromJSON Button where
	parseJSON = withObject "Button" $ \v ->
		Button <$> v .: "text" <*> parseJSON (Object v)

instance ToJSON Button where
	toJSON (Button text (Open url)) = object
		["text" .= text, "url" .= url]
	toJSON (Button text (Callback cbd)) = object
		["text" .= text, "callback_data" .= cbd]

data Pressed = Open Text | Callback Text deriving Show

instance FromJSON Pressed where
	parseJSON = withObject "Pressed" $ \v ->
		(is_open v <|> is_callback v) >>= maybe
			(fail "Action on pressing isn't set") return where

		is_open, is_callback :: Object -> Parser (Maybe Pressed)
		is_open v = (fmap . fmap) Open $ v .:? "url"
		is_callback v = (fmap . fmap) Callback $ v .:? "callback_data"

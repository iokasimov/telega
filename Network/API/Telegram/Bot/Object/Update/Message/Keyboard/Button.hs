module Network.API.Telegram.Bot.Object.Update.Message.Keyboard.Button (Button (..), Pressed (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON)
	, Object, Value (Object), object, withObject, (.:), (.:?), (.=))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Applicative (pure, (<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function ((.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Maybe (Maybe, maybe)
import "base" Text.Show (Show)
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
			(fail "Action on pressing isn't set") pure where

		is_open, is_callback :: Object -> Parser (Maybe Pressed)
		is_open v = (fmap . fmap) Open $ v .:? "url"
		is_callback v = (fmap . fmap) Callback $ v .:? "callback_data"

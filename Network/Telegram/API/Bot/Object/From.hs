module Network.Telegram.API.Bot.Object.From (From (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Object, withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Parser)
import "base" Data.Bool (bool)
import "text" Data.Text (Text)

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

type Whom = Int -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> From

instance FromJSON From where
	parseJSON = withObject "From" $ \v -> v .: "is_bot"
		>>= bool (from v User) (from v Bot) where

		from :: Object -> Whom -> Parser From
		from v f = f <$> v .: "id"
			<*> v .:? "username"
			<*> v .: "first_name"
			<*> v .:? "last_name"
			<*> v .:? "language_code"

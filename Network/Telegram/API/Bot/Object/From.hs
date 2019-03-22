module Network.Telegram.API.Bot.Object.From (From (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:), (.:?))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

instance FromJSON From where
	parseJSON (Object v) = v .: "is_bot" >>= \case
		True -> Bot
			<$> v .: "id"
			<*> v .:? "username"
			<*> v .: "first_name"
			<*> v .:? "last_name"
			<*> v .:? "language_code"
		False -> User
			<$> v .: "id"
			<*> v .:? "username"
			<*> v .: "first_name"
			<*> v .:? "last_name"
			<*> v .:? "language_code"

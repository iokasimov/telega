module Network.Telegram.API.Bot.Object.Callback (Callback (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Message (Message)

data Callback = Datatext Message Text deriving Show

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "message" <*> v .: "data"

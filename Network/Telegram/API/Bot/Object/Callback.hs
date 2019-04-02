module Network.Telegram.API.Bot.Object.Callback (Callback (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Data.Int (Int)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Message (Message)

data Callback = Datatext Int Message Text deriving Show

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"

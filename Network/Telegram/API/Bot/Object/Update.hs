module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))

import Network.Telegram.API.Bot.Object.Message (Message)

data Update = Incoming Int Message deriving Show

instance FromJSON Update where
	parseJSON = withObject "Incoming" $ \v ->
		Incoming <$> v .: "update_id" <*> v .: "message"

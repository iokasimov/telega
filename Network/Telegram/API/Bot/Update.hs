module Network.Telegram.API.Bot.Update (Update) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))

import Network.Telegram.API.Bot.Message (Message)

data Update
	= Incoming Int Message

instance FromJSON Update where
	parseJSON (Object v) = Incoming
		<$> v .: "update_id"
		<*> v .: "message"

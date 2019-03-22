module Network.Telegram.API.Bot.Object.Update (Update (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))

import Network.Telegram.API.Bot.Object.Message (Message)

data Update
	= Incoming Int Message
	deriving Show

instance FromJSON Update where
	parseJSON (Object v) = Incoming
		<$> v .: "update_id"
		<*> v .: "message"

module Network.API.Telegram.Bot.Object.Chat.Channel (Channel (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Data.Function (($))
import "base" Text.Show (Show)
import "base" Data.Functor ((<$>))

import Network.API.Telegram.Bot.Field (Title)

data Channel = Channel Title
	deriving Show

instance FromJSON Channel where
	parseJSON = withObject "Channel" $
		\chat -> Channel <$> chat .: "title"

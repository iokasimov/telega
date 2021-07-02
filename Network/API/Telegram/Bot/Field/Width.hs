module Network.API.Telegram.Bot.Field.Width (Width) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Width = Width Int
	deriving Show

instance Accessible Int Width where
	access f (Width int) = (\int' -> Width int') <$> f int

instance FromJSON Width where
	parseJSON o = Width <$> parseJSON o

instance ToJSON Width where
	toJSON (Width d) = toJSON d

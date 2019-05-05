module Network.API.Telegram.Bot.Field.Height (Height) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Height = Height Int

instance Accessible Int Height where
	access f (Height int) = (\int' -> Height int') <$> f int

instance FromJSON Height where
	parseJSON o = Height <$> parseJSON o

instance ToJSON Height where
	toJSON (Height d) = toJSON d
